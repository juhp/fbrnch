module Krb (
  fasIdFromKrb,
  maybeFasIdFromKrb,
  krbTicket
  ) where

import Common

import SimpleCmd

krbTicket :: IO ()
krbTicket = do
  krb <- klistEntryFedora
  if null krb
    then error' "No krb5 ticket found for FEDORAPROJECT.ORG"
    else
    when (last krb == "(Expired)") $ do
      putStrLn $ unwords krb
      cmd_ "fkinit" []
      putStrLn ""

maybeFasIdFromKrb :: IO (Maybe String)
maybeFasIdFromKrb =
  fmap (removeSuffix "@FEDORAPROJECT.ORG") . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) <$> klistEntryFedora

fasIdFromKrb :: IO String
fasIdFromKrb = do
  mfasid <- maybeFasIdFromKrb
  case mfasid of
    Nothing -> error' "Could not determine fasid from klist"
    Just fasid -> return fasid

klistEntryFedora :: IO [String]
klistEntryFedora = do
  mres <- cmdMaybe "klist" ["-l"]
  return $ case mres of
    Nothing ->  []
    Just out -> (words . fromMaybe "" . find ("@FEDORAPROJECT.ORG" `isInfixOf`) . lines) out
