module Client where


import Storage
import Backup
import System.Environment (getArgs)
import Data.Char (toLower, digitToInt)
import System.IO (isEOF)
import System.Directory (copyFile)
import System.FilePath ((</>))

functions = zip [1..] [addFiles, manualBackup, restoreFiles, viewFiles, removeFiles]

main = do
  args <- getArgs
  if "-i" `elem` args
     then interractive
     else error "Only '-i' (interractive) mode is implemented for now."

interractive :: IO ()
interractive = do
  greetings >> putStr "\n" >> menu
  c <- getChar
  getChar
  maybe (putStrLn "Choice is incorrect, please enter a value within range" >> interractive) id (lookup (digitToInt c) functions)

greetings :: IO ()
greetings = mapM_ putStrLn
            ["=== Welcome in the (soon) automated backup software ===\n"
           , "\tYou are in the interractive mode, it is the easiest way of using this sofware, just follow the instructions onscreen and everything should be fine: they are designed to be straight and usable by the most of people."
            ]

menu :: IO ()
menu = mapM_ putStrLn
       [ "Please make you choice: what do you want to do?"
       , "1. Add files for schedule backup"
       , "2. Issue a general backup"
       , "3. Restore a file"
       , "4. View files scheduled for backup"
       , "5. Remove a file from schedule backup"
       ]

-- Have files added to the schedules
addFiles :: IO ()
addFiles = mapM_ getFiles [Hourly, Daily, Weekly, Monthly]

-- To me mapped over each backup policy
getFiles :: BackupPolicy -> IO ()
getFiles pol = do
  putStrLn $ "Give the list of files you want to schedule for a " ++ (map toLower . show $ pol) ++ " backup. Finish by an empty line."
  getUserFiles >>= (flip store pol)

-- Get a list of file paths entered by the user from the command line. Terminated by empty line.
getUserFiles :: IO [FilePath]
getUserFiles = do
  l <- getLine
  if null l
     then return []
     else do
       a <- getUserFiles
       return (l : a)

{- Doesn't work yet, have to figure out why
emptyBuffer :: IO ()
emptyBuffer = do
  b <- isEOF
  if b
     then 
     else 
-}

manualBackup :: IO ()
manualBackup = mapM_ issueBackup (map fst dispatch)

restoreFiles :: IO ()
restoreFiles = do
  restoreGreetings
  l <- getUserFiles
  mapM_ (\f -> copyFile (backups </> (removeRoot f)) f ) l

viewFiles = undefined -- To be implemented
removeFiles = undefined -- To be implemented

restoreGreetings :: IO ()
restoreGreetings = mapM_ putStrLn
                   [ "Welcome in the file restore process, be warned: there is not (yet) a preview featured, which means whichever file you enter will be restored, provided it exists in the backup hierarchy."
                   , "Enter the filenames you want to restore, one at a time, end with a blank line"
                   ]