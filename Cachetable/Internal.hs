{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

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
import Data.Foldable (toList)
import Data.Traversable (for)
import Data.List (unfoldr)
import Control.Monad (when)
import Text.Printf (printf)
import GHC.Stack (HasCallStack)

-- TODO: can we unbox this into the storage?
-- | Container for an entry in the cache.
data Entry key val
  = EntryNotPresent
  | EntryPresent
    { entry_key :: !key
    , entry_value :: !val
    }
  deriving (Show, Eq)

-- | Is the 'Entry' an 'EntryPresent'
isEntryPresent :: Entry a b -> Bool
isEntryPresent EntryPresent{} = True
isEntryPresent _ = False

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
  , ct_salt :: !Int
    -- ^ salt to use for hashing
  , ct_generator :: MutVar s StdGen
    -- ^ random number generator to use when replacing something in the cache
    -- when inserting
  }

-- | Display internal structure of the cache.
showCachetable
  :: forall key val m . (PrimMonad m, Show key, Show val)
  => Cachetable (PrimState m) key val
  -> m String
showCachetable Cachetable{..} = do
  arr <- freezeArray ct_storage 0 (ct_buckets * ct_bucketSlots)
  generator <- readMutVar ct_generator
  pure (unlines
    [ "Cachetable"
    , "  { storage = " <> show arr
    , "  , buckets = " <> show ct_buckets
    , "  , bucketSlots = " <> show ct_bucketSlots
    , "  , salt = " <> show ct_salt
    , "  , generator = " <> show generator
    , "  }"
    ])

-- | Internal invariants.
--
-- - A bucket should only have 'EntryNotPresent' entires at the end.
-- - A 'EntryPresent' in a bucket should have a key that hashes to the bucket
--   index.
-- - A bucket should only have one entry with the same key
--
checkInvariants
  :: forall key val m . (HasCallStack, PrimMonad m, Hashable key, Eq key)
  => Cachetable (PrimState m) key val
  -> m ()
checkInvariants Cachetable{..} = do
  arr <- freezeArray ct_storage 0 (ct_buckets * ct_bucketSlots)
  let entries = toList arr
  let
    buckets =
      unfoldr (\mrest -> case splitAt ct_bucketSlots <$> mrest of
        Nothing -> Nothing
        Just (bucket, []) -> Just (bucket, Nothing)
        Just (bucket, rest) -> Just (bucket, Just rest))
      (Just entries)

  _ <- for (zip [0..] buckets) \(bucketIdx, bucket) ->

    for (zip [0..] bucket) \(entryIdx, entry) -> case entry of
      EntryNotPresent -> do
        let (_, rest) = splitAt entryIdx bucket
        -- A bucket should only have 'EntryNotPresent' entires at the end.
        when (any isEntryPresent rest)
          (error "EntryPresent after EntryNotPresent")

      EntryPresent{entry_key = key} -> do
        let hash = hashWithSalt ct_salt key
        let bucketIdx' = hash `mod` ct_buckets

        -- A 'EntryPresent' in a bucket should have a key that hashes to the
        -- bucket index.
        when (bucketIdx /= bucketIdx')
          (error
            (printf "bucket index does not agree, %d, %d" bucketIdx bucketIdx'))

        -- A bucket should only have one entry with the same key
        let
          sameKey entry = case entry of
            EntryNotPresent -> False
            EntryPresent{..} -> key == entry_key
        let same = filter sameKey bucket
        when (map entry_key same /= [key])
          (error "multiple entries with the same key")
  pure ()

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
  (salt, generator) <- pure (uniform generator)
  generator <- newMutVar generator

  pure Cachetable
    { ct_storage = storage
    , ct_buckets = buckets
    , ct_bucketSlots = bucketSlots
    , ct_salt = salt
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
  let !hash = hashWithSalt ct_salt key
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
  let !hash = hashWithSalt ct_salt key
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
