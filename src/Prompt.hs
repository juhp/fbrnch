module Prompt (
  prompt,
  prompt_
  ) where

import Data.Char (isPrint)

import Common
import Common.System

-- import System.Console.Haskeline
import System.IO

-- FIXME promptNonEmpty
prompt :: String -> IO String
prompt s = do
  -- -- doesn't work in emacs-vterm :(
  -- runInputT defaultSettings loop
  --  where
  --      loop :: InputT IO String
  --      loop = do
  --          minput <- getInputLine $ s ++ ": "
  --          case minput of
  --              Nothing -> return ""
  --              Just input -> return input
  putStr $ s ++ ": "
  tty <- openFile "/dev/tty" ReadMode
  inp <- hGetLine tty
  if all isPrint inp
    then return inp
    else do
    warning $ "input rejected because of unprintable character(s): " ++
      filter (not . isPrint) inp
    prompt s

prompt_ :: String -> IO ()
prompt_ = void <$> prompt
