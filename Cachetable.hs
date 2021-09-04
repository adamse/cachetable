-- | A hash table suitable for use in caches.
module Cachetable
  ( Cachetable.Internal.Cachetable()
  , Cachetable.Internal.new
  , Cachetable.Internal.lookup
  , Cachetable.Internal.insert
  ) where

import qualified Cachetable.Internal
