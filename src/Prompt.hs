module Prompt (
  prompt,
  prompt_
  ) where

import Common

prompt :: String -> IO String
prompt s = do
  putStr $ s ++ ": "
  inp <- getLine
  putStrLn ""
  return inp

prompt_ :: String -> IO ()
prompt_ = void <$> prompt
