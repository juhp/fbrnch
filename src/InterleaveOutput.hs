module InterleaveOutput (cmdSilent', cmdSilentBool) where

import Data.ByteString.Lazy.UTF8 as B
import GHC.IO.Exception (ExitCode(ExitSuccess))
import System.Process.Typed (proc, readProcessInterleaved)
import Common

cmdSilent' :: String -> [String] -> IO ()
cmdSilent' c args =
  void $ cmdSilentBool c args

cmdSilentBool :: String -> [String] -> IO Bool
cmdSilentBool c args = do
  (ret, out) <- readProcessInterleaved (proc c args)
  let ok = ret == ExitSuccess
  unless ok $ do
    putStrLn ""
    putStrLn (B.toString out)
  return ok
