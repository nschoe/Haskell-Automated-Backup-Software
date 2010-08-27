{- File:         Storage.hs
   Version:      0.1
   Provides:     Storage
   Synopsis:     Provides a very basic library that sets "global variables",
                 defines the data structures, keep shedule files up to date.
-}

module Storage (
                 Settings(..)
               , BackupPolicy(..)
               , addForSchedule
               , mapBP
               , getSettings
               , removeRoot
               ) where

import Control.Monad (filterM)
import Data.List (union, sort, isPrefixOf)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), isPathSeparator, takeDirectory, )


-- For dev' purpose only, will be removed
{-devSettings = Settings "/home/nschoe/backups/software"
                       "/home/nschoe/backups/software/schedule/hourly"
                       "/home/nschoe/backups/software/schedule/daily"
                       "/home/nschoe/backups/software/schedule/weekly"
                       "/home/nschoe/backups/software/schedule/monthly"
                       "/home/nschoe/backups/software/backups"
                       "/home/nschoe/backups/software/dates"
-}

{-| Define settings type. |-}
data Settings = Settings {
    getWorkspace :: FilePath -- root of the backup disk
  , getHourly    :: FilePath -- hourly schedule file
  , getDaily     :: FilePath -- daily schedule file
  , getWeekly    :: FilePath -- weekly schedule file
  , getMonthly   :: FilePath -- monthly schedule file
  , getBackups   :: FilePath -- root directory for storing backuped files
  , getDates     :: FilePath -- file in which backup dates are stored
}

{-| Define the backup policy each file is attached to. |-}
data BackupPolicy = Hourly | Daily | Weekly | Monthly
                  deriving (Eq, Show, Read)

{-| Parse a file containig the settings for the software, mainly paths to backups location and workspace.
Needs great improvement, because it is too strict (use real parse intead) and uses 'error' which is bad |-}
getSettings :: FilePath -> IO (Settings)
getSettings f = do
  contents <- readFile f                           -- needs to be improved: use exception proof, such as try, or catch or equivalent
  let contents' = filter (not . isPrefixOf "#") (lines contents) -- eliminates lines begining with "#" -> allow comments.
  if length contents' /= 7
    then error "Your settings file is not filled in correctly, please be sure to read the documentation to know how to fill your file"
    else return $ Settings (contents'!!0) (contents'!!1) (contents'!!2) (contents'!!3) (contents'!!4) (contents'!!5) (contents'!!6)

{-| Add a list of files for scheduled backup |-}
addForSchedule :: Settings -> BackupPolicy -> [FilePath] -> IO ()
addForSchedule settings pol xs = do
  files <- filterM doesFileExist xs
  let sche = mapBP pol $ settings
  old <- readFile sche                              -- needs to be improved: use exception proof, such as try, or catch or equivalent
  let new = sort $ files `union` lines old
  updateHierarchy settings new                     -- newly added files may be in new directories
  writeFile sche (unlines new)
         

{-| Maintains the internal hierarchy inside 'getBackups' identical to the user's hard drive. |-}
updateHierarchy :: Settings -> [FilePath] -> IO ()
updateHierarchy settings = mapM_ (createDirectoryIfMissing True . (</>) (getBackups settings) . removeRoot . takeDirectory)

{-| Extract the schedule file based on the backup policy it is given. |-}  
mapBP :: BackupPolicy -> (Settings -> FilePath)
mapBP Hourly  = getHourly
mapBP Daily   = getDaily
mapBP Weekly  = getWeekly
mapBP Monthly = getMonthly

{-| Remove the root folder in either Windows (C:,D:,...) or Linux (/) of a FilePath. |-}
removeRoot :: FilePath -> FilePath
removeRoot [] = []  -- useless, but for consistency sake
removeRoot ('/':xs) = xs -- linux root
removeRoot l@(x:xs) = if x `elem` windowsDrives && head xs == ':'
                        then dropWhile isPathSeparator $ tail xs
                        else l
  where windowsDrives = ['A'..'Z']