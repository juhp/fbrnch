module Prompt (
  prompt,
  prompt_,
  yesno
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

-- from dnf-repo Sudo.hs
yesno :: String -> IO Bool
yesno desc = do
  inp <- prompt $ desc ++ "? [y/n]"
  case lower inp of
    "y" -> return True
    "yes" -> return True
    "n" -> return False
    "no" -> return False
    _ ->  yesno desc
