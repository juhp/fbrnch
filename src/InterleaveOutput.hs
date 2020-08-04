{-# LANGUAGE CPP #-}

module InterleaveOutput (cmdSilent_) where

import System.Process.Typed (proc, readProcessInterleaved_)

import Common

cmdSilent_ :: String -> [String] -> IO ()
cmdSilent_ c args =
  void $ readProcessInterleaved_ $ proc c args
