module Repoquery (
  repoquery
  )
where

import Data.Maybe (isJust)
import SimpleCmd

import Bodhi (bodhiTestingRepo)
import Branches

-- FIXME use fedora-repoquery library
-- FIXME default to package
repoquery :: Branch -> Branch -> [String] -> IO String
repoquery sysbr br args = do
  mtesting <- bodhiTestingRepo br
  let brOpts =
        if sysbr == br
        then []
        else do
          case br of
            Rawhide -> ["--disablerepo=*", "--enablerepo=rawhide"]
            Fedora _ -> ["--disablerepo=*", "--enablerepo=fedora"] ++
                        ["--enablerepo=updates-testing" | isJust mtesting] ++
                        ["--releasever=" ++ branchVersion br]
            EPEL _ -> ["--disablerepo=*", "--enablerepo=epel"] ++
                      ["--enablerepo=epel-testing" | isJust mtesting] ++
                      ["--releasever=" ++ branchVersion br]
            EPELNext _ -> ["--disablerepo=*", "--enablerepo=epel-next"] ++
                          ["--enablerepo=epel-next-testing" | isJust mtesting] ++
                          ["--releasever=" ++ branchVersion br]
  cmd "dnf" (["repoquery", "--quiet"] ++ brOpts ++ args)
