module Visualization
  ( displayFa
  ) where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.Process (callProcess)
import System.IO (hClose, openTempFile)
import Types.Fa

-- |Displays a window with a diagram representation of a FA.
displayFa :: (Show sym, Show sta) => Fa sym sta -> IO ()
displayFa fa = do
  tempDirectory <- getTemporaryDirectory
  (dotFilePath, dotFilehandle) <- openTempFile tempDirectory "automata.gv"
  (imageFilePath, imageFilehandle) <- openTempFile tempDirectory "automata.png"
  hClose dotFilehandle
  hClose imageFilehandle
  saveDotToImageFile dotFilePath imageFilePath (faToDot fa)
  displayImage imageFilePath
  removeFile dotFilePath
  removeFile imageFilePath

saveDotToImageFile :: FilePath -> FilePath -> String -> IO ()
saveDotToImageFile dotFilePath imageFilePath dotContent = do
  writeFile dotFilePath dotContent
  callProcess "dot" ["-Tpng", dotFilePath, "-o", imageFilePath]

faToDot :: (Show sym, Show sta) => Fa sym sta -> String
faToDot (Fa initialStates finalStates transitions) =
  "digraph something { node [shape=circle] {" ++
  initialStatesToDot initialStates ++
  finalStatesToDot finalStates ++
  "}" ++
  transitionsToDot transitions ++
  "}"
    where

      initialStatesToDot =
        concatMap (\state -> showInQuotesWithSuffix "_start" state
          ++ " [shape=point];"
          ++ showInQuotesWithSuffix "_start" state
          ++ "->"
          ++ showInQuotes state
          ++ ";")

      finalStatesToDot =
        concatMap (\state -> showInQuotes state ++ " [shape=doublecircle];")

      transitionsToDot =
        concatMap mapper

      mapper (Transition symbol state finalState) =
        showInQuotes state ++ " -> " ++ showInQuotes finalState ++ " [ label=" ++ show symbol ++ " ];"

showInQuotesWithSuffix :: Show a => String -> a -> String
showInQuotesWithSuffix suffix object =
  "\"" ++ content ++ "\""
    where
      content = (filter (/= '\"') . (++ suffix) . show) object

showInQuotes :: Show a => a -> String
showInQuotes =
  showInQuotesWithSuffix ""

displayImage :: FilePath -> IO ()
displayImage imagePath = do
  imageExists <- doesFileExist imagePath
  if not imageExists
    then putStrLn $ "Cannot find file " ++ imagePath
    else do
      initGUI
      window <- windowNew
      window `on` deleteEvent $ liftIO mainQuit >> return False
      scroll <- scrolledWindowNew Nothing Nothing
      scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
      vbox <- vBoxNew False 0
      hbox <- hBoxNew False 0
      set window [ windowDefaultWidth := 500
        , windowDefaultHeight := 500
        , containerChild := vbox
        , containerBorderWidth := 0 ]
      image <- imageNew
      boxPackEnd vbox hbox PackNatural 0
      boxPackStart vbox scroll PackGrow 0
      scrolledWindowAddWithViewport scroll image
      imageSetFromFile image imagePath
      widgetShowAll window
      mainGUI
