module Prompt (
  prompt,
  prompt_,
  refPrompt,
  conflictPrompt
  ) where

import Common

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
  if "\ESC[" `isInfixOf` inp
    then prompt s
    else return inp

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

-- FIXME select ref by number
refPrompt :: [String] -> String -> IO (Maybe (Maybe String))
refPrompt commits txt = do
  let commitrefs = tail $ map (head . words) commits
  ref <- prompt txt
  if null ref then return (Just Nothing) else
    if lower ref == "no" then return Nothing
    else if ref `elem` commitrefs
      then return $ Just (Just ref)
      else refPrompt commits txt

-- FIXME also include branch
conflictPrompt :: [String] -> String -> IO (Maybe String)
conflictPrompt commits txt = do
  let commitrefs = map (head . words) commits
  ref <- prompt txt
  if null ref then return Nothing
    else if ref `elem` commitrefs
      then return $ Just ref
      else conflictPrompt commits txt
