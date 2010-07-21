
import Storage
import System.FilePath ((</>))
import System.Time (ClockTime(..), getClockTime, TimeDiff(..), addToClockTime)
import Backup
import Data.List (findIndices)
import Control.Concurrent (forkIO)
import System.IO (putChar)

data Instruction = Start | Stop | GeneralBackup
                   deriving (Eq, Show, Read)

pipe = workspace </> "pipeFile"
oneHour = TimeDiff {tdYear=0, tdMonth=0,tdDay=0,tdHour=1,tdMin=0,tdSec=0,tdPicosec=0}
oneDay =  TimeDiff {tdYear=0, tdMonth=0,tdDay=1,tdHour=0,tdMin=0,tdSec=0,tdPicosec=0}
oneWeek =  TimeDiff {tdYear=0, tdMonth=0,tdDay=7,tdHour=0,tdMin=0,tdSec=0,tdPicosec=0}
oneMonth =  TimeDiff {tdYear=0, tdMonth=1,tdDay=0,tdHour=0,tdMin=0,tdSec=0,tdPicosec=0}
toAdd = [(Hourly, oneHour), (Daily, oneDay), (Weekly, oneWeek), (Monthly, oneMonth)]

main = do
  now <- getClockTime
  forkIO (timer Hourly (addToClockTime (TimeDiff {tdYear=0, tdMonth=0,tdDay=0,tdHour=0,tdMin=1,tdSec=0,tdPicosec=0}) now))
  --forkIO (timer Daily (addToClockTime oneDay now))
  --forkIO (timer Weekly (addToClockTime oneWeek now))
  --forkIO (timer Monthly (addToClockTime oneMonth now))


checkPipe :: IO (Maybe [Instruction])
checkPipe = do
  cont <- readFile pipe
  if null cont
     then return Nothing
     else do
       return (Just $ map read (lines cont))

timer :: BackupPolicy -> ClockTime -> IO ()
timer pol time = do
  now <- getClockTime
  if now >= time
     then do
       issueBackup pol
       let delay = maybe oneDay id (lookup pol toAdd)
       timer pol (addToClockTime delay time)
     else timer pol time