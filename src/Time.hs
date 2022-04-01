module Time (
  timeIO
  )
where

import Control.Exception
import Data.Time.Clock

timeIO :: IO a -> IO a
timeIO action = do
  bracket
    getCurrentTime
    (\start -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ "took " ++ show duration)
    (const action)
