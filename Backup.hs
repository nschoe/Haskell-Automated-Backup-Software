module Backup (
                issueBackup
              ) where

import Storage
import System.Directory (copyFile)
import System.FilePath ((</>), isPathSeparator)

{-| Backup (i.e. copy into internal architecture) a list of files |-}
backup :: Settings -> [FilePath] -> IO ()
backup settings = mapM_ (\f -> copyFile f ((getBackups settings) </> (removeRoot f)))

issueBackup :: Settings -> BackupPolicy -> IO ()
{-issueBackup settings p = do
    let a = mapBP p $ settings
    toBackup <- readFile a
    backup (lines toBackup)-}
    
issueBackup settings p = readFile (mapBP p $ settings) >>= (backup settings . lines)