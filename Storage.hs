module Storage (
                 store
               , removeRoot
               , BackupPolicy(..)
               , workspace
               , daily
               , hourly
               , weekly
               , monthly
               , dispatch
               , backups
               ) where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.List (sort, union, nub)
import System.FilePath ((</>), takeDirectory, pathSeparator, isPathSeparator)
import System.IO (IOMode(..), readFile, writeFile)
import Control.Monad (filterM)

workspace = "/Users/nschoe/Documents/Storage"
daily = workspace </> "schedule" </> "daily"
hourly = workspace </> "schedule" </> "hourly"
weekly = workspace </> "schedule" </> "weekly"
monthly = workspace </> "schedule" </> "monthly"

dispatch :: [(BackupPolicy, FilePath)]
dispatch = [(Daily, daily), (Hourly, hourly), (Weekly, weekly), (Monthly, monthly)]

backups = workspace </> "backups"

data BackupPolicy = Monthly | Weekly | Daily | Hourly
                    deriving (Eq, Show, Read)

-- Take a list of files, keep the existing ones, sort them, update the hierarchy, and add them to the appropriate schedule file.
store :: [FilePath] -> BackupPolicy -> IO ()
store xs pol = do
  files <- filterM doesFileExist xs
  let sche = maybe daily id (lookup pol dispatch)
  existing <- readFile sche
  let updated = unlines . sort $ files `union` lines existing
  updated `seq` (updateHierarchy (lines updated) >> writeFile sche updated)

-- Update the hard disk print in the backup directory
updateHierarchy :: [FilePath] -> IO ()
updateHierarchy = mapM_ (createDirectoryIfMissing True . (</>) backups  . removeRoot . takeDirectory)

removeRoot :: FilePath -> FilePath
removeRoot = dropWhile isPathSeparator