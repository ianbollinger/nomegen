-- Copyright 2015 Ian D. Bollinger
--
-- Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
-- http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
-- or http://opensource.org/licenses/MIT>, at your option. This file may not be
-- copied, modified, or distributed except according to those terms.

{-# LANGUAGE CPP #-}

module Main (
    main,
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Char (toUpper)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp ((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Nomegen (
    Nomicon, explainYamlParseException, generate, nameToText, yamlDeserializer,
    )

data Application = Application {
    nameLabel :: Gtk.Label,
    fileChooserButton :: Gtk.FileChooserButton,
    generateButton :: Gtk.Button,
    nomicon :: IORef (Maybe Nomicon)
    }

main :: IO ()
main = do
    _ <- Gtk.initGUI
    app <- Application
        <$> buildNameLabel
        <*> buildFileChooserButton
        <*> buildGenerateButton
        <*> newIORef Nothing
    window <- buildWindow app
    Gtk.widgetShowAll window
    Gtk.mainGUI

buildWindow :: Application -> IO Gtk.Window
buildWindow app = do
    window <- Gtk.windowNew
    _ <- Gtk.on window Gtk.objectDestroy Gtk.mainQuit
    Gtk.set window [
        Gtk.containerBorderWidth := 10,
        Gtk.windowTitle := "nomegen"
        ]
    connectSignals app
    hBox <- Gtk.hBoxNew False 10
    Gtk.containerAdd hBox (fileChooserButton app)
    Gtk.containerAdd hBox (generateButton app)
    vBox <- Gtk.vBoxNew False 10
    Gtk.boxPackStart vBox (nameLabel app) Gtk.PackGrow 0
    Gtk.boxPackStart vBox hBox Gtk.PackNatural 0
    Gtk.containerAdd window vBox
    return window

connectSignals :: Application -> IO ()
connectSignals app = do
    _ <- Gtk.on (fileChooserButton app) Gtk.fileSelectionChanged $
        onFileSelectionChanged app
    _ <- Gtk.on (generateButton app) Gtk.buttonActivated $
        onGenerateButtonActivated (nameLabel app) (nomicon app)
    return ()

onFileSelectionChanged :: Application -> IO ()
onFileSelectionChanged app = do
    Gtk.set (generateButton app) [Gtk.widgetSensitive := True]
    fileName' <- Gtk.fileChooserGetFilename (fileChooserButton app)
    let fileName = fromMaybe (error "impossible") fileName'
    x <- yamlDeserializer fileName
    case x of
        Left err -> error (explainYamlParseException err)
        Right lexicon' -> writeIORef (nomicon app) (Just lexicon')

onGenerateButtonActivated :: Gtk.Label -> IORef (Maybe Nomicon) -> IO ()
onGenerateButtonActivated label lexicon = do
    lexicon' <- readIORef lexicon
    case lexicon' of
        Just lexicon'' -> do
            name <- getName lexicon''
            Gtk.set label [
                Gtk.labelLabel := Text.pack "<span size=\"xx-large\">"
                    <> name <> Text.pack "</span>",
                Gtk.labelSelectable := True
                ]
        Nothing -> error "impossible"

buildNameLabel :: IO Gtk.Label
buildNameLabel = do
    label <- Gtk.labelNew (Just "<span size=\"xx-large\"> </span>")
    Gtk.set label [Gtk.labelUseMarkup := True]
    return label

buildGenerateButton :: IO Gtk.Button
buildGenerateButton = do
    button <- Gtk.buttonNew
    Gtk.set button [
        Gtk.buttonLabel := "Generate",
        Gtk.widgetSensitive := False
        ]
    return button

buildFileChooserButton :: IO Gtk.FileChooserButton
buildFileChooserButton = do
    button <-
        Gtk.fileChooserButtonNew "Select nomicon" Gtk.FileChooserActionOpen
    fileFilter <- buildFileFilter
    Gtk.set button [Gtk.fileChooserFilter := fileFilter]
    return button

buildFileFilter :: IO Gtk.FileFilter
buildFileFilter = do
    fileFilter <- Gtk.fileFilterNew
    Gtk.set fileFilter [Gtk.fileFilterName := "nomicon file"]
    Gtk.fileFilterAddPattern fileFilter "*.yaml"
    Gtk.fileFilterAddPattern fileFilter "*.yml"
    Gtk.fileFilterAddPattern fileFilter "*.nomicon"
    return fileFilter

getName :: Nomicon -> IO Text
getName nomicon' = capitalize . nameToText <$> generate nomicon'

capitalize :: Text -> Text
capitalize = Text.cons . toUpper . Text.head <*> Text.tail
