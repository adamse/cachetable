{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_HADDOCK not-home #-}

-- {-# OPTIONS_GHC -ddump-cmm -ddump-to-file #-}

-- | Implementation details for 'Cachetable'.
module Cachetable.Internal
  ( module Cachetable.Internal
  ) where

import Data.Primitive.Array
import Data.Primitive.MutVar
import Control.Monad.Primitive
import Data.Hashable
import System.Random
import Data.Tuple (swap)
import Control.Exception (assert)

-- | A salt to use for hashing.
--
-- TODO: how to choose a good one?
hashSalt :: Int
hashSalt = 143898437
{-# inline hashSalt #-}

-- TODO: can we unbox this into the storage?
-- | Container for an entry in the cache.
data Entry key val
  = EntryNotPresent
  | EntryPresent
    { entry_key :: !key
    , entry_value :: !val
    }
  deriving (Show)

-- | A cache table (hash table but keys are allowed to dissappear).
--
-- As described in <https://fgiesen.wordpress.com/2019/02/11/cache-tables/>.
--
-- The table is WHNF strict in keys and values.
--
-- 'insert' and 'lookup' are both linear in time in the @bucketSlots@.
data Cachetable s key val = Cachetable
  { ct_storage :: !(MutableArray s (Entry key val))
    -- ^ storage
  , ct_buckets :: !Int
    -- ^ number of buckets
  , ct_bucketSlots :: !Int
    -- ^ slots per bucket
  , ct_generator :: MutVar s StdGen
    -- ^ random number generator for use when replacing something in the cache
    -- when inserting
  }

-- | Display internal structure of the cache.
showCachetable
  :: forall key val m . (PrimMonad m, Show key, Show val)
  => Cachetable (PrimState m) key val
  -> m String
showCachetable Cachetable{..} = do
  arr <- freezeArray ct_storage 0 (ct_buckets * ct_bucketSlots)
  pure (unlines
    [ "Cachetable"
    , "  { storage = " <> show arr
    , "  , buckets = " <> show ct_buckets
    , "  , bucketSlots = " <> show ct_bucketSlots
    ])

-- | Create a new 'Cachetable'.
--
-- The total capacity is @buckets * bucketSlots@.
new
  :: forall key val m . PrimMonad m
  => Int
    -- ^ @buckets@. Number of buckets in cache, must be greater than 0.
  -> Int
    -- ^ @bucketSlots@. Number of slots in each bucket, must be greater than
    -- 0.
  -> m (Cachetable (PrimState m) key val)
new buckets bucketSlots = do
  assert (buckets > 0) (pure ())
  assert (bucketSlots > 0) (pure ())

  storage <- newArray (buckets * bucketSlots) EntryNotPresent

  generator <- unsafeIOToPrim newStdGen
  generator <- newMutVar generator

  pure Cachetable
    { ct_storage = storage
    , ct_buckets = buckets
    , ct_bucketSlots = bucketSlots
    , ct_generator = generator
    }

-- | Lookup a value in the cache.
lookup
  :: forall key val m . (PrimMonad m, Eq key, Hashable key)
  => Cachetable (PrimState m) key val
  -> key
    -- ^ Key to find.
  -> m (Maybe val)
lookup Cachetable{..} !key = do
  let !hash = hashWithSalt hashSalt key
  let !bucket = hash `mod` ct_buckets
  let !bucket_start = bucket * ct_bucketSlots
  let !bucket_end = (bucket + 1) * ct_bucketSlots
  let
    -- try to find the key in the bucket
    search !idx
      | idx == bucket_end = pure Nothing
      | otherwise = do
        entry <- readArray ct_storage idx
        case entry of
          EntryNotPresent -> pure Nothing
          EntryPresent{..}
            | entry_key == key -> pure (Just entry_value)
          _ -> search (idx + 1)

  search bucket_start

-- | Insert a value into the cache.
--
-- - If the key is already in the cache the value will be replaced.
-- - If the bucket the key would go into is already full a random slot in that
--   bucket will be replaced.
insert
  :: forall key val m . (PrimMonad m, Eq key, Hashable key)
  => Cachetable (PrimState m) key val
  -> key
  -> val
  -> m Bool
    -- ^ @True@ if something was replaced when inserting.
insert Cachetable{..} !key !val = do
  let !hash = hashWithSalt hashSalt key
  let !bucket = hash `mod` ct_buckets
  let !bucket_start = bucket * ct_bucketSlots
  let !bucket_end = (bucket + 1) * ct_bucketSlots
  let
    -- Find the index to insert at
    search :: Int -> m (Bool, Int)
    search !idx
      -- We are at the end of the bucket, which means there were no empty slots
      -- to use. Select a random slot in the bucket to replace.
      | idx == bucket_end = do
        !slot <-
          atomicModifyMutVar'
            ct_generator
            (swap . uniformR (0, ct_bucketSlots - 1))
        pure (True, bucket_start + slot)

      | otherwise = do
        entry <- readArray ct_storage idx
        case entry of
          -- We found an empty slot in the bucket, use it.
          EntryNotPresent -> pure (False, idx)
          -- We found the key in the bucket, replace the value.
          EntryPresent{..}
            | entry_key == key -> pure (True, idx)
          -- Otherwise look at the next slot.
          _ -> search (idx + 1)

  (!replace, !idx) <- search bucket_start
  let !entry = EntryPresent { entry_key = key, entry_value = val }
  writeArray ct_storage idx entry
  pure replace
