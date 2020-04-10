module Prompt where

import Common

prompt :: String -> IO String
prompt s = do
  putStr $ "Press Enter " ++ s ++ ": "
  inp <- getLine
  putStrLn ""
  return inp

prompt_ :: String -> IO ()
prompt_ = void <$> prompt
