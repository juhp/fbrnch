import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

import Control.Monad (when)
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

main :: IO ()
main =
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    run <$> strArg "FROMBRANCH" <*> strArg "TOBRANCH"

run :: String -> String -> IO ()
run frombrnch tobrnch = do
  fedpkg "switch-branch" [tobrnch]
  git_ "diff" [tobrnch, frombrnch]
  git_ "merge" [frombrnch]
  tty <- hIsTerminalDevice stdin
  when tty $ do
    hSetBuffering stdout NoBuffering
    putStr "Press Enter to push"
    getLine >> return ()
  fedpkg "push" []
  fedpkg "build" []
  --waitFor build
  --cmd_ "bodhi" ["updates", "new", build]
  --when override $ cmd_ "bodhi" ["overrides", "save", build]

fedpkg :: String -> [String] -> IO ()
fedpkg c args =
  cmd_ "fedpkg" (c:args)
