{-# LANGUAGE CPP #-}

module InterleaveOutput (cmdSilent', cmdSilentBool) where

import Data.ByteString.Lazy.UTF8 as B
import GHC.IO.Exception (ExitCode(ExitSuccess))
import System.Process.Typed (proc, readProcessInterleaved,
                             readProcessInterleaved_)
import Common

cmdSilent' :: String -> [String] -> IO ()
cmdSilent' c args =
  void $ readProcessInterleaved_ $ proc c args

cmdSilentBool :: String -> [String] -> IO Bool
cmdSilentBool c args = do
  (ret, out) <- readProcessInterleaved (proc c args)
  let ok = ret == ExitSuccess
  unless ok $ putStrLn (B.toString out)
  return ok
