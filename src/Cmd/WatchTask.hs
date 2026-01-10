module Cmd.WatchTask (
  watchTaskCmd,
  )
where

import Numeric.Natural
import Distribution.Koji (TaskID(TaskId))


-- import Branches
-- import Common.System
-- import Git
import Koji
-- import Package
-- import Types

-- FIXME support watching buildArch
watchTaskCmd :: Natural -> IO ()
watchTaskCmd taskid = do
  kojiWatchParentTask fedoraHub (TaskId $ fromIntegral taskid)
