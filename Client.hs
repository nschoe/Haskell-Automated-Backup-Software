{- File:         Client.hs
   Version:      0.1
   Author:       nschoe (ns.schoe@gmail.com)
   Maintainer:   nschoe (ns.schoe@gmail.com)
   Provides:     Client (the executable is Client_Main.hs)
   Portability:  Linux (tested on Archlinux) / Windows (tested on Windows seven)
   Synopsis:     A simple Gtk2+ GUI client interface for the backup software. Allow a user
                 to perform the usual basic commands such as adding files for schedule                 
                 backup, or issueing a general backup without using the command line.
-}

module Client where

import Backup
import Storage
import Control.Monad (mapM_, (>>))
import Data.Char (toLower)
import Data.IORef
import Graphics.UI.Gtk

data BackupAction = AddFiles (Maybe BackupPolicy)
            | ViewFiles (Maybe BackupPolicy)
            | RestoreFiles (Maybe BackupPolicy)
            | RemoveFiles (Maybe BackupPolicy)
            | Menu
              deriving (Eq)

{-| The fonction is passed the file which contains the settings for our application. |-}
main :: FilePath -> IO ()
main f = do
  settings <- getSettings f
  state <- newIORef Menu
  
  initGUI
  
-- Windows creations
  mainWindow <- windowNew
  set mainWindow [windowTitle := "Haskell Automated Backup Software", windowDefaultWidth := 150,
                  windowDefaultHeight := 200, containerBorderWidth := 10]
  infoWindow <- windowNew
  set infoWindow [windowTitle := "Information", windowDefaultWidth := 130, windowDefaultHeight := 35, containerBorderWidth := 10]
  fcWindow      <- windowNew
  fcWidget      <- fileChooserWidgetNew FileChooserActionOpen
  set fcWindow [windowTitle := "Choose files...", windowDefaultWidth := 500,
                windowDefaultHeight := 400, containerBorderWidth := 8]
  policyWindow  <- windowNew
  set policyWindow [windowTitle := "Policy chooser", windowDefaultWidth := 100,
                    windowDefaultHeight := 80, containerBorderWidth := 4]
  displayWindow <- windowNew
  set displayWindow [windowTitle := "Files viewer", windowDefaultWidth := 450,
                     windowDefaultHeight := 350, containerBorderWidth := 10]
  scrledWindow  <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrledWindow PolicyAutomatic PolicyAutomatic
  
-- Buttons creation
  addButton <- buttonNewWithLabel "Add files for scheduled backups"
  viewButton <- buttonNewWithLabel "View files schedulded for backups"
  restoreButton <- buttonNewWithLabel "Restore files from backup"
  removeButton <- buttonNewWithLabel "Remove files from scheduled backups"
  issueButton <- buttonNewWithLabel "Issue a general backup"
  quitButton <- buttonNewWithLabel "Quit"
  
  selectButton <- buttonNewWithLabel "Select"
  cancelButton <- buttonNewWithLabel "Cancel"
  
  hourlyButton <- buttonNewWithLabel "Hourly"
  dailyButton <- buttonNewWithLabel "Daily"
  weeklyButton <- buttonNewWithLabel "Weekly"
  monthlyButton <- buttonNewWithLabel "Monthly"
  
  displayOkButton <- buttonNewWithLabel "     Ok     "                                 -- Yeah, not very wise, to be changed.
  
-- Misc
  infoLabel <- labelNew (Just "Choose the action you want to do.")
  fileChooserSetSelectMultiple fcWidget True
  displayLabel <- labelNew (Just "Here are the files scheduled for your policy:")
  hSep1 <- hSeparatorNew
  displayText <- textViewNew
  
-- Layout settlement
  mainVBox <- vBoxNew True 5
  mainHSep <- hSeparatorNew
  boxPackStart mainVBox addButton PackNatural 0
  boxPackStart mainVBox viewButton PackNatural 0
  boxPackStart mainVBox restoreButton PackNatural 0
  boxPackStart mainVBox removeButton PackNatural 0
  boxPackStart mainVBox issueButton PackNatural 0
  boxPackStart mainVBox mainHSep PackNatural 0
  boxPackStart mainVBox quitButton PackNatural 0
  
  containerAdd mainWindow mainVBox
  
  containerAdd infoWindow infoLabel
  
  fcHBox <- hBoxNew False 5
  boxPackEnd fcHBox cancelButton PackNatural 0
  boxPackEnd fcHBox selectButton PackNatural 0
  fileChooserSetExtraWidget fcWidget fcHBox
  containerAdd fcWindow fcWidget
  
  policyVBox <- vBoxNew True 5
  boxPackStart policyVBox hourlyButton PackNatural 0
  boxPackStart policyVBox dailyButton PackNatural 0
  boxPackStart policyVBox weeklyButton PackNatural 0
  boxPackStart policyVBox monthlyButton PackNatural 0
  containerAdd policyWindow policyVBox
  
  displayHBox <- hBoxNew False 0
  boxPackEnd displayHBox displayOkButton PackNatural 0
  displayVBox <- vBoxNew False 10
  boxPackStart displayVBox displayLabel PackNatural 0
  boxPackStart displayVBox hSep1 PackNatural 0
  boxPackStart displayVBox scrledWindow PackGrow 0
  boxPackStart displayVBox displayHBox PackNatural 0
  scrolledWindowAddWithViewport scrledWindow displayText
  containerAdd displayWindow displayVBox
  
-- Signals connection
  onDestroy mainWindow (myQuit [infoWindow,fcWindow,policyWindow,displayWindow])
  onClicked quitButton (widgetDestroy mainWindow)
  
  onClicked addButton (addFunction state infoLabel policyWindow)
  onClicked viewButton (viewFunction state infoLabel policyWindow)
  mapM_ (\b -> onClicked b ((policyFunction settings state infoLabel b fcWindow displayWindow displayText)
                            >> (widgetHide policyWindow))) [hourlyButton, dailyButton, weeklyButton, monthlyButton] 
  onClicked issueButton ((issueAllBackups settings) >> (giveInfo infoLabel "All of your schedulded files have been backed up!"))
  onClicked removeButton (giveInfo infoLabel "Sorry, feature not implemented yet!")
  onClicked restoreButton (giveInfo infoLabel "Sorry, feature not implemented yet!")
  
  onClicked cancelButton ((giveInfo infoLabel "Choose the action you want to do.") >> (writeIORef state Menu) >> (widgetHide fcWindow))
  onClicked selectButton ((selectFunction settings state infoLabel fcWidget) >> (widgetHide fcWindow))
  onClicked displayOkButton ((giveInfo infoLabel "Choose the action you want to do.") >> (widgetHide displayWindow))
  
-- Launching the GUI
  widgetShowAll infoWindow
  widgetShowAll mainWindow
  mainGUI
  
myQuit :: [Window] -> IO ()
myQuit l = do
  mapM_ widgetDestroy l
  mainQuit
  
addFunction :: IORef BackupAction -> Label -> Window -> IO ()
addFunction st info policyWindow = do
  giveInfo info "To which policy do you wish do add you files?"
  writeIORef st $ AddFiles Nothing
  widgetShowAll policyWindow

viewFunction :: IORef BackupAction -> Label -> Window -> IO ()
viewFunction st info policyWindow = do
  giveInfo info "The files for which policy do you want to view?"
  writeIORef st $ ViewFiles Nothing
  widgetShowAll policyWindow
  
selectFunction :: Settings -> IORef BackupAction -> Label -> FileChooserWidget -> IO ()
selectFunction settings state info widget = do
  AddFiles (Just pol) <- readIORef state        -- Maybe a bit risky, but if designed well, the select button can only be clicked over adding files
  files <- fileChooserGetFilenames widget
  addForSchedule settings pol files
  let nb = length files
      m  = case nb of
        1 -> "1 file "
        _ -> show nb ++ " files "
  giveInfo info $ m ++ "added for " ++ map toLower (show pol) ++ " backups."

policyFunction :: Settings -> IORef BackupAction -> Label -> Button -> Window -> Window -> TextView -> IO ()
policyFunction settings state info button fcWindow dWindow tv = do
  action <- readIORef state
  policy <- get button buttonLabel
  let policy' = read policy :: BackupPolicy
  case action of
    AddFiles _ -> do
      writeIORef state (AddFiles (Just policy'))
      giveInfo info $ "Select the files you want to add for " ++ map toLower policy ++ " backups.\n'Ctrl' for multiple selection."
      widgetShowAll fcWindow
    ViewFiles _ -> do
      writeIORef state (ViewFiles (Just policy'))
      giveInfo info $ "Here are the files scheduled for " ++ map toLower policy ++ " backups."
      viewFiles settings info state tv
      widgetShowAll dWindow
    RestoreFiles _ -> do
      giveInfo info "Feature not implemented yet."
  
giveInfo :: Label -> String -> IO ()
giveInfo label str = do
  set label [labelLabel := str]

viewFiles :: Settings -> Label -> IORef BackupAction -> TextView -> IO ()
viewFiles settings info state tv = do
  ViewFiles (Just pol) <-readIORef state
  files <- getScheduldedFiles settings pol
  buf <- textViewGetBuffer tv
  textBufferSetText buf (unlines files)
  textViewSetBuffer tv buf
  let nb = length files
      m = case nb of
        1 -> "1 file "
        _ -> show nb ++ " files "
  giveInfo info $ m ++ "are schedulded for " ++ map toLower (show pol) ++ " backups."