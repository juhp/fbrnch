{-# LANGUAGE CPP #-}

module InterleaveOutput (cmdSilent_) where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Exit (exitWith, ExitCode (..))
import System.Process (showCommandForUser)
import System.Process.Typed (proc, readProcessInterleaved)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

cmdSilent_ :: String -> [String] -> IO ()
cmdSilent_ c args = do
  (ret, out) <- readProcessInterleaved $ proc c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> do
      B.putStrLn $ B.pack (showCommandForUser c args ++ " failed with status " ++ show n ++ "\n") <> out
      exitWith $ ExitFailure n
