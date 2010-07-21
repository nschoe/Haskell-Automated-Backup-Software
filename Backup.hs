 module Backup (
                issueBackup
               ) where

import Storage
import System.Directory (copyFile)
import System.FilePath ((</>))

-- Backup a list of files
backup :: [FilePath] -> IO ()
backup = mapM_ (\f -> copyFile f (backups </> (removeRoot f)))

issueBackup :: BackupPolicy -> IO ()
issueBackup p = do
  let a = maybe (error "Policy not specified correctly.") id (lookup p dispatch)
  toIssue <- readFile a
  backup (lines toIssue)