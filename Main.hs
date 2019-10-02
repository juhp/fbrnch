module Main (main) where

import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

main :: IO ()
main =
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    run <$> strArg "FROMBRANCH" <*> strArg "TOBRANCH"

run :: String -> String -> IO ()
run frombrnch tobrnch = do
  fedpkg "switch-branch" [tobrnch]
  git_ "diff" [frombrnch]
  git_ "merge" [frombrnch]
  fedpkg "push" []
  fedpkg "build" []

fedpkg :: String -> [String] -> IO ()
fedpkg c args =
  cmd_ "fedpkg" (c:args)
