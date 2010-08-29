{- File:         Backup.hs
   Version:      0.1
   Author:       nschoe (ns.schoe@gmail.com)
   Maintainer:   nschoe (ns.schoe@gmail.com)
   Provides      Backup
   Portability:  Linux (tested on Archlinux) / Windows (tested on Windows seven)
   Synopsis:     Provides a very basic library that handles the details of backing files up,
                 i.e. removing the root identifier and copying them into the internal
                 architecture.
-}

module Backup (
                issueBackup
              , issueAllBackups
              ) where

import Storage
import Control.Monad (mapM_)
import System.Directory (copyFile)
import System.FilePath ((</>), isPathSeparator)

{-| Backup (i.e. copy into internal architecture) a list of files |-}
backup :: Settings -> [FilePath] -> IO ()
backup settings = mapM_ (\f -> copyFile f ((getBackups settings) </> (removeRoot f)))

issueBackup :: Settings -> BackupPolicy -> IO ()
issueBackup settings p = readFile (mapBP p $ settings) >>= (backup settings . lines)

issueAllBackups :: Settings -> IO ()
issueAllBackups settings = mapM_ (issueBackup settings) [Hourly, Daily, Weekly, Monthly]
