module InterleaveOutput (cmdSilent', cmdSilentBool) where

import Data.ByteString.Lazy.UTF8 as B
import GHC.IO.Exception (ExitCode(ExitSuccess))
import System.Process.Typed (proc, readProcessInterleaved)
import Common
import Common.System

cmdSilent' :: String -> [String] -> IO ()
cmdSilent' c args = do
  ok <- cmdSilentBool c args
  unless ok $ error' $ unwords (c:args) ++ ": failed"

-- currently unused
cmdSilentBool :: String -> [String] -> IO Bool
cmdSilentBool c args = do
  (ret, out) <- readProcessInterleaved (proc c args)
  let ok = ret == ExitSuccess
  unless ok $ do
    putNewLn
    putStrLn (B.toString out)
  return ok
