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

getBook :: FilePath -> Text -> IO Book
getBook bookDir bookName = do 
  filesForBook <- listDirectory (bookDir </> "Section With Pause")
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

showWindow :: IORef [Book] -> IO ()
showWindow booksRef = do 
  Gtk.init Nothing
  scrolledWindow <- new Gtk.ScrolledWindow []
  win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
                        , #iconName := "applications-haskell"
                        , #defaultWidth := 300
                        , #defaultHeight := 600
                        , #child := scrolledWindow
                        ]
  booksBox <- Gtk.boxNew Gtk.OrientationVertical 10
  #add scrolledWindow booksBox
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
      on fileCheck #toggled $ do 
        toPlay <- Gtk.get fileCheck #active
        updateToPlay booksRef (bookName book) file toPlay
      
  -- Pressing play button starts playback and changes to a stop button.    
  -- If any entry is (un)ticked, save the listing of files to play to the disk.
  -- Pressing play button starts playback and changes to a stop button.

  on win #destroy Gtk.mainQuit
  #setTitle win "Tango Player"

  #showAll win

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  Gtk.main

main :: IO ()
main = do 
  -- Load the previous listing of what files to play.
  possibleFromXDG <- getBooksFromXDGDir
  -- Scan the Anki Tango folders for each book and grab the section based mp3 files.
  books <- maybe getBooksFromTangoDir pure possibleFromXDG
  booksRef <- newIORef books
  showWindow booksRef

  -- writeBooksToXDGDir books
