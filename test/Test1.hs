{-# LANGUAGE TypeApplications #-}

import qualified Cachetable.Internal as Cachetable
import Data.Maybe (isNothing)
import GHC.Stack (HasCallStack)

assert :: HasCallStack => Bool -> String -> IO ()
assert True _ = pure ()
assert False err = error err

main :: IO ()
main = do
  ct <- Cachetable.new @Int @Int 1 1
  Cachetable.checkInvariants ct

  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 1
  Cachetable.checkInvariants ct
  assert (isNothing val) "value is there but shouldn't be"

  replace <- Cachetable.insert ct 1 1
  Cachetable.checkInvariants ct
  assert (not replace) "we shouldn't have replaced anything"

  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 1
  Cachetable.checkInvariants ct
  assert (val == Just 1) "we couldn't extract what we just put in"

  Cachetable.showCachetable ct >>= putStrLn

  replace <- Cachetable.insert ct 1 2
  Cachetable.checkInvariants ct
  assert replace "we should've replaced something"

  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 1
  Cachetable.checkInvariants ct
  assert (val == Just 2) "we couldn't extract what we just put in"

  ct <- Cachetable.new @Int @Int 1 2
  Cachetable.checkInvariants ct

  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 1
  Cachetable.checkInvariants ct
  assert (isNothing val) "value is there but shouldn't be"

  replace <- Cachetable.insert ct 1 1
  Cachetable.checkInvariants ct
  assert (not replace) "we shouldn't have replaced anything"

  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 1
  Cachetable.checkInvariants ct
  assert (val == Just 1) "we couldn't extract what we just put in"

  Cachetable.showCachetable ct >>= putStrLn

  replace <- Cachetable.insert ct 2 2
  Cachetable.checkInvariants ct
  assert (not replace) "we shouldn't have replaced something"

  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 1
  Cachetable.checkInvariants ct
  assert (val == Just 1) "we couldn't extract first value"

  val <- Cachetable.lookup ct 2
  Cachetable.checkInvariants ct
  assert (val == Just 2) "we couldn't extract second value"
  Cachetable.showCachetable ct >>= putStrLn

  replace <- Cachetable.insert ct 2 3
  Cachetable.checkInvariants ct
  assert replace "we should have replaced something"
  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 2
  Cachetable.checkInvariants ct
  assert (val == Just 3) "we couldn't extract second value"

  replace <- Cachetable.insert ct 3 3
  Cachetable.checkInvariants ct
  assert replace "we should have replaced something"
  Cachetable.showCachetable ct >>= putStrLn

  val <- Cachetable.lookup ct 3
  Cachetable.checkInvariants ct
  assert (val == Just 3) "we couldn't extract the third value"
