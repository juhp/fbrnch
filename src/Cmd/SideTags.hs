module Cmd.SideTags (sideTagsCmd) where

import SimpleCmd (cmd_)

import Branches
import Common
import Koji
import Krb (krbTicket)
import Prompt (yesno)

sideTagsCmd :: Bool -> [Branch] -> IO ()
sideTagsCmd remove brs = do
  sidetags <-
    if null brs
    then kojiUserSideTags Nothing
    else concat <$> mapM (kojiUserSideTags . Just) brs
  when remove krbTicket
  mapM_ (if remove then removeSideTag else putStrLn) sidetags
  where
    removeSideTag :: String -> IO ()
    removeSideTag tag =
      whenM (yesno $ "Remove " ++ tag) $
      cmd_ "fedpkg" ["remove-side-tag", tag]
