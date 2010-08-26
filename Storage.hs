{- File:         Storage.hs
   Version:      0.1
   Provides:     Storage
   Synopsis:     Provides a very basic library that sets "global variables",
                 defines the data structures, keep shedule files up to date.
-}

module Storage (
                 workspace
               ) where

import System.FilePath ((</>))

workspace = "/home/nschoe/backups/software"
hourly    = workspace </> "schedule" </> "daily"