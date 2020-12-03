module Prompt (
  prompt,
  prompt_,
  refPrompt
  ) where

import Common

import Data.Char

-- FIXME promptNonEmpty
prompt :: String -> IO String
prompt s = do
  putStr $ s ++ ": "
  getLine

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

-- FIXME select ref by number
refPrompt :: [String] -> String -> IO (Maybe (Maybe String))
refPrompt commits txt = do
  let commitrefs = tail $ map (head . words) commits
  inp <- prompt txt
  if null inp then return (Just Nothing) else
    if map toLower inp == "no" then return Nothing
    else case find (inp `isPrefixOf`) commitrefs of
      Just ref -> return $ Just (Just ref)
      Nothing -> refPrompt commits txt
