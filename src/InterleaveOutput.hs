{-# LANGUAGE CPP #-}

module InterleaveOutput (cmdSilent_) where

import System.Process.Typed (proc, readProcessInterleaved_)

import Common

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

cmdSilent_ :: String -> [String] -> IO ()
cmdSilent_ c args =
  void $ readProcessInterleaved_ $ proc c args
