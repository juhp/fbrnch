module Prompt (
  prompt,
  prompt_,
  refPrompt,
  conflictPrompt
  ) where

import Common

import Data.Char
import System.IO

-- FIXME promptNonEmpty
prompt :: String -> IO String
prompt s = do
  putStr $ s ++ ": "
  tty <- openFile "/dev/tty" ReadMode
  inp <- hGetLine tty
  putStrLn ""
  return inp

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

-- FIXME select ref by number
refPrompt :: [String] -> String -> IO (Maybe (Maybe String))
refPrompt commits txt = do
  let commitrefs = tail $ map (head . words) commits
  ref <- prompt txt
  if null ref then return (Just Nothing) else
    if map toLower ref == "no" then return Nothing
    else if ref `elem` commitrefs
      then return $ Just (Just ref)
      else refPrompt commits txt

conflictPrompt :: [String] -> String -> IO (Maybe String)
conflictPrompt commits txt = do
  let commitrefs = tail $ map (head . words) commits
  ref <- prompt txt
  if null ref then return Nothing
    else if ref `elem` commitrefs
      then return $ Just ref
      else conflictPrompt commits txt
