{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Data.Hashable
import Data.List
import Control.Monad
import Data.HashMap.Strict as SM
import Data.Monoid
import System.Directory
import System.FilePath
import System.IO
import Data.Aeson
import GHC.Generics
import System.XDG
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.IORef
import Data.Text hiding (zip)
import Control.Lens
import qualified Control.Lens as L
import Data.Generics.Product
import Data.Generics.Sum
import System.Process
import Control.Concurrent

type ProcessValues = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) 

data Book = Book
          { bookName :: Text
          , files :: SM.HashMap FilePath Bool
          }
          deriving (Eq, Show, Generic)

instance ToJSON Book where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Book

configFilePath :: FilePath
configFilePath = "tango-player/books.json"

baseBookDirectory :: FilePath
baseBookDirectory = "/home/sean/Books/Japan/JLPT Tango/"

sectionDirPart :: FilePath
sectionDirPart = "Section With Pause"

mpvOptions :: [String]
mpvOptions = ["--no-video"]

getBook :: FilePath -> Text -> IO Book
getBook bookDir bookName = do 
  filesForBook <- listDirectory (bookDir </> sectionDirPart)
  let files = SM.fromList $ fmap (\file -> (file, False)) filesForBook
  pure Book{..}

getBooksFromTangoDir :: IO [Book]
getBooksFromTangoDir = do 
  bookDirs <- listDirectory baseBookDirectory
  unsortedBooks <- traverse (\bookDir -> getBook (baseBookDirectory </> bookDir) (pack bookDir)) bookDirs
  pure $ sortOn bookName unsortedBooks

getBooksFromXDGDir :: IO (Maybe [Book])
getBooksFromXDGDir = do 
  fromConfigFile <- readConfigFile configFilePath 
  pure $ do 
    configFileBytes <- fromConfigFile
    decode configFileBytes

writeBooksToXDGDir :: [Book] -> IO ()
writeBooksToXDGDir books = do
  configHome <- getConfigHome
  createDirectoryIfMissing True $ takeDirectory (configHome </> configFilePath)
  writeConfigFile configFilePath $ encode books

updateBooksToPlay :: Text -> FilePath -> Bool -> [Book] -> [Book]
updateBooksToPlay bookNameToTarget file play books =
  L.set (traverse . filtered (\Book{..} -> bookName == bookNameToTarget) . field @"files" . at file) (Just play) books

updateToPlay :: IORef [Book] -> Text -> FilePath -> Bool -> IO ()
updateToPlay booksRef bookNameToTarget file toPlay = do 
  modifyIORef booksRef $ updateBooksToPlay bookNameToTarget file toPlay
  books <- readIORef booksRef
  writeBooksToXDGDir books

filesToPlay :: IORef [Book] -> IO [FilePath]
filesToPlay booksRef = do 
  books <- readIORef booksRef
  pure $ sort $ do 
    book <- books 
    let nameOfBook = bookName book
    fileToPlay <- SM.keys $ SM.filter id $ files book
    pure (baseBookDirectory </> unpack nameOfBook </> sectionDirPart </> fileToPlay)

stopPlaying :: IORef (Maybe ProcessValues) -> IO ()
stopPlaying playingProcessValuesRef = do 
  maybeProcessValues <- readIORef playingProcessValuesRef
  case maybeProcessValues of 
    Nothing -> pure ()
    Just (_, _, _, processHandle) -> terminateProcess processHandle
  writeIORef playingProcessValuesRef Nothing

startPlaying :: IORef [Book] -> IORef (Maybe ProcessValues) -> IO ()
startPlaying booksRef playingProcessValuesRef = do 
  stopPlaying playingProcessValuesRef
  files <- filesToPlay booksRef
  print ("mpv", (mpvOptions <> files))
  let processDescriptor = proc "mpv" (mpvOptions <> files)
  processValues <- createProcess processDescriptor
  writeIORef playingProcessValuesRef (Just processValues)

createPlayBar :: IORef [Book] -> IORef (Maybe ProcessValues) -> IO Gtk.ActionBar
createPlayBar booksRef playingProcessValuesRef = do 
  playBar <- new Gtk.ActionBar []
  stopButton <- new Gtk.Button [ #label := "Stop"
                               ]
  #packStart playBar stopButton
  on stopButton #clicked $ do 
    stopPlaying playingProcessValuesRef
      
  playButton <- new Gtk.Button [ #label := "Play"
                               ]
  #packStart playBar playButton
  on playButton #clicked $ do 
    startPlaying booksRef playingProcessValuesRef

  pure playBar

createListing :: IORef [Book] -> IO Gtk.ScrolledWindow
createListing booksRef = do 
  scrolledListing <- new Gtk.ScrolledWindow []
  booksBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := 10
                          ]
  #add scrolledListing booksBox
  -- Display listing of files with checkboxes against them.  
  startingBooks <- readIORef booksRef
  forM_ startingBooks $ \book -> do
    bookFrame <- new Gtk.Frame [ #label := bookName book
                               , #marginStart := 10
                               , #marginEnd := 10
                               ]
    #add booksBox bookFrame
    filesGrid <- new Gtk.Grid [ #rowSpacing := 4
                              , #columnSpacing := 3
                              , #marginStart := 10
                              , #marginEnd := 10
                              , #marginLeft := 10
                              , #marginRight := 10
                              ]
    #add bookFrame filesGrid
    let fileEntries = sort $ SM.toList $ files book
    let fileEntriesAndIndexes = zip fileEntries [0..]
    forM_ fileEntriesAndIndexes $ \((file, checked), rowIndex) -> do
      fileLabel <- new Gtk.Label [ #label := pack file
                                 ]
      #attach filesGrid fileLabel 0 rowIndex 1 1
      fileCheck <- new Gtk.CheckButton [ #active := checked
                                       , #marginTop := 2
                                       , #marginLeft := 10
                                       ]
      #attach filesGrid fileCheck 1 rowIndex 1 1
      -- If any entry is (un)ticked, save the listing of files to play to the disk.
      on fileCheck #toggled $ do 
        toPlay <- Gtk.get fileCheck #active
        updateToPlay booksRef (bookName book) file toPlay
  pure scrolledListing

showWindow :: IORef [Book] -> IORef (Maybe ProcessValues) -> IO ()
showWindow booksRef playingProcessValuesRef = do 
  Gtk.init Nothing
  win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
                        , #iconName := "applications-haskell"
                        , #defaultWidth := 260
                        , #defaultHeight := 600
                        ]
  mainBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 10
                         ]
  #add win mainBox
  
  bookListingBox <- createListing booksRef
  #packStart mainBox bookListingBox True True 0
  
  playBar <- createPlayBar booksRef playingProcessValuesRef
  #packEnd mainBox playBar False False 0
      
  on win #destroy $ do
    stopPlaying playingProcessValuesRef
    Gtk.mainQuit
  #setTitle win "Tango Player"

  #showAll win

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  Gtk.main

main :: IO ()
main = do 
  playingProcessValuesRef <- newIORef Nothing
  -- Load the previous listing of what files to play.
  possibleFromXDG <- getBooksFromXDGDir
  -- Scan the Anki Tango folders for each book and grab the section based mp3 files.
  books <- maybe getBooksFromTangoDir pure possibleFromXDG
  booksRef <- newIORef books
  showWindow booksRef playingProcessValuesRef
