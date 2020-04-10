module Krb where

import Common

import SimpleCmd

krbTicket :: IO ()
krbTicket = do
  krb <- words . fromMaybe "" . find ("@FEDORAPROJECT.ORG" `isInfixOf`) . lines <$> cmd "klist" ["-l"]
  if null krb
    then error' "No krb5 ticket found for FEDORAPROJECT.ORG"
    else
    when (last krb == "(Expired)") $ do
      putStrLn $ unwords krb
      cmd_ "kinit" [head krb]

fasIdFromKrb :: IO String
fasIdFromKrb = do
  mfasid <- (removeSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
  case mfasid of
    Nothing -> error' "Could not determine fasid from klist"
    Just fasid -> return fasid
