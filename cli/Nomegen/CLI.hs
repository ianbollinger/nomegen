-- Copyright 2015 Ian D. Bollinger
--
-- Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
-- http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
-- or http://opensource.org/licenses/MIT>, at your option. This file may not be
-- copied, modified, or distributed except according to those terms.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Nomegen.CLI (
    main,
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM_)
import Data.Char (toUpper)
import Data.Monoid ((<>))
import System.IO (Handle, IOMode (WriteMode), openFile, stdout)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative.Builder as Opt
import Options.Applicative.Common (Parser)
import Options.Applicative.Extra (execParser, helper)

import Nomegen (
    Nomicon, explainYamlParseException, generate, nameToText, yamlDeserializer,
    )

data Options = Options {
    _output :: !(IO Handle),
    number :: !Int,
    lowerCase :: !Bool,
    lexiconFile :: !String
    }

main :: IO ()
main = do
    x <- execParser opts
    yamlDeserializer (lexiconFile x) >>= \case
        Left err -> putStrLn $ explainYamlParseException err
        Right lexicon' -> printWords x lexicon'
  where
    opts = Opt.info (helper <*> options)
        ( Opt.fullDesc
        <> Opt.progDesc "stuff"
        <> Opt.header "other"
        )

printWords :: Options -> Nomicon -> IO ()
printWords options' nomicon =
    replicateM_ (number options') $
        -- TODO: don't hard code 7.
        Text.putStrLn . format . nameToText =<< generate 7 nomicon
  where
    format
        | lowerCase options' = id
        | otherwise = capitalize

options :: Parser Options
options = Options
    -- TODO: translate IO exception into Left arm of EitherT.
    <$> Opt.option (flip openFile WriteMode <$> Opt.str)
        ( Opt.short 'o'
        <> Opt.long "out"
        <> Opt.help "file to write the output"
        <> Opt.value (return stdout)
        )
    <*> Opt.option Opt.auto
        ( Opt.short 'n'
        <> Opt.long "number"
        <> Opt.help "number of words to generate (default: 1)"
        <> Opt.value (1 :: Int)
        )
    <*> Opt.switch
        ( Opt.short 'l'
        <> Opt.long "lowercase"
        <> Opt.help "do not capitalize output"
        )
    -- TODO: this should be stdin by default.
    <*> Opt.strArgument
        ( Opt.metavar "FILE"
        )

capitalize :: Text -> Text
capitalize = Text.cons . toUpper . Text.head <*> Text.tail

