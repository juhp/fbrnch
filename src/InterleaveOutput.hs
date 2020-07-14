module InterleaveOutput (cmdSilent_) where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Exit (ExitCode (..))
import System.Process (showCommandForUser)
import System.Process.Typed (proc, readProcessInterleaved)

import Common.System

cmdSilent_ :: String -> [String] -> IO ()
cmdSilent_ c args = do
  (ret, out) <- readProcessInterleaved $ proc c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error' $ showCommandForUser c args +-+ "failed with status" +-+ show n ++ "\n" ++ B.unpack out
