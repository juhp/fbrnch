module Prompt (
  prompt,
  prompt_,
  refPrompt,
  conflictPrompt
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

-- FIXME select ref by number
refPrompt :: [String] -> String -> IO (Maybe (Maybe String))
refPrompt commits txt = do
  let commitrefs = tail $ map (head . words) commits
  ref <- prompt txt
  case lower ref of
    "" -> return $ Just Nothing
    "no" -> return Nothing
    "n" -> return Nothing
    _ -> if ref `elem` commitrefs
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
      else if lower ref == "head"
           then return $ Just $ head commitrefs
           else conflictPrompt commits txt
