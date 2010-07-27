module Main () where

import Storage
import Backup
import System.FilePath ((</>))
import Data.Time (getCurrentTime, UTCTime(..), secondsToDiffTime)
import Data.List (intersperse)
import Control.Concurrent (threadDelay)
import System.IO (readFile, writeFile)

delay = 1000000 * 30 :: Int

-- 'main' must be recoded, now it is only for purely functional goals, nothing beautiful here.
main = do
  l <- checkDates
  l' <- mapM perform l
  writeFile dates $ concat (intersperse "\n" (map show l'))  
  threadDelay delay
  main

checkDates :: IO ([(BackupPolicy, UTCTime)])
checkDates = do
  c <- readFile dates
  let c' = map read $ lines c
  return (zipWith ((,)) [Hourly, Daily, Weekly, Monthly] c')

-- Needs to be recoded, it's unfathomly ugly in here
perform :: (BackupPolicy, UTCTime) -> IO (UTCTime)
perform (pol, time) = do
  now <- getCurrentTime
  if now >= time
     then do
       issueBackup pol
       return (update (pol, now))
     else
       return time
    where update (Hourly,t) = t {utctDayTime = utctDayTime t + secondsToDiffTime 60} -- 3600
          update (Daily,t) = t {utctDayTime = utctDayTime t + secondsToDiffTime 86400}
          update (Weekly,t) = t {utctDayTime = utctDayTime t + secondsToDiffTime 604800}
          update (Monthly,t) = t {utctDayTime = utctDayTime t + secondsToDiffTime 2419200}