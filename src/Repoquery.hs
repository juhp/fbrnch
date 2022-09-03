module Repoquery (
  repoquery
  )
where

import Data.Maybe (isJust)
import SimpleCmd

import Bodhi (bodhiTestingRepo)
import Branches

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
                        ["--enablerepo=updates"| isJust mtesting] ++
                        ["--releasever=" ++ branchVersion br]
            EPEL _ -> ["--disablerepo=*", "--enablerepo=epel",
                         "--releasever=" ++ branchVersion br]
            EPELNext _ -> ["--disablerepo=*", "--enablerepo=epel-next",
                         "--releasever=" ++ branchVersion br]
  cmd "dnf" (["repoquery", "--quiet"] ++ brOpts ++ args)
