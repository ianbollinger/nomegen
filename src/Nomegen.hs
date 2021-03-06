-- Copyright 2015 Ian D. Bollinger
--
-- Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
-- http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
-- or http://opensource.org/licenses/MIT>, at your option. This file may not be
-- copied, modified, or distributed except according to those terms.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: $Header$
-- Description: Random name generation
-- Copyright: 2015 Ian D. Bollinger
-- License: MIT or Apache 2.0
-- Maintainer: Ian D. Bollinger <ian.bollinger@gmail.com>
-- Stability: unstable
-- Portability: portable
module Nomegen (
    -- * Nomicon
    Nomicon (..),
    nomiconNames,
    nomiconSegments,
    leastSegments,
    shortestName,
    mostSegments,
    longestName,

    -- * Name
    Name,
    makeName,
    nameToText,

    -- * Segment
    Segment,
    makeSegment,
    segmentToText,

    -- * Deserialization
    yamlDeserializer,
    explainYamlParseException,

    -- * Generation
    generate,
    ) where

import Control.Monad.ST (ST)
import qualified Data.Foldable as Foldable
#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif
import Data.Monoid ((<>))
import Data.Ord (comparing)

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Data.Set (Set)
import System.Random.MWC (GenST, asGenST, withSystemRandom)
import System.Random.MWC.CondensedTable (genFromTable)

import Nomegen.Internal (
    MarkovElement (MarkovFinal, MarkovInitial, MarkovMedial),
    MarkovMap (MarkovMap),
    Nomicon (_nomiconEntries, _nomiconMarkovMap, _nomiconSegments), Name (Name),
    Segment (Segment), nameToText,
    )

-- Nomicon ---------------------------------------------------------------------

-- | A set of every 'Segment' contained in each 'Name' in the given 'Nomicon'.
nomiconSegments :: Nomicon -> Set Segment
nomiconSegments = _nomiconSegments

nomiconNames :: Nomicon -> Seq Name
nomiconNames = _nomiconEntries

-- | The length of the shortest 'Name' in a 'Nomicon' in segments.
leastSegments :: Nomicon -> Int
leastSegments = Seq.length . nameSegments . shortestName

-- | The shortest 'Name' in a 'Nomicon'.
shortestName :: Nomicon -> Name
shortestName =
    Foldable.minimumBy (comparing $ Seq.length . nameSegments) . nomiconNames

-- | The length of the longest 'Name' in a 'Nomicon' in segments.
mostSegments :: Nomicon -> Int
mostSegments = Seq.length . nameSegments . longestName

-- | The longest 'Name' in a 'Nomicon'.
longestName :: Nomicon -> Name
longestName =
    Foldable.maximumBy (comparing $ Seq.length . nameSegments) . nomiconNames

-- Name ------------------------------------------------------------------------

makeName :: Seq Segment -> Name
makeName = Name

nameSegments :: Name -> Seq Segment
nameSegments (Name x) = x

-- Segment ---------------------------------------------------------------------

makeSegment :: Text -> Segment
makeSegment = Segment

segmentToText :: Segment -> Text
segmentToText (Segment x) = x

-- Serialization ---------------------------------------------------------------

yamlDeserializer :: FilePath -> IO (Either Yaml.ParseException Nomicon)
yamlDeserializer = Yaml.decodeFileEither

explainYamlParseException :: Yaml.ParseException -> String
explainYamlParseException = (preamble <>) . \case
    Yaml.NonScalarKey ->
        "Non-scalar key."
    Yaml.UnknownAlias anchorName ->
        "Unknown alias: " <> anchorName <> "."
    Yaml.UnexpectedEvent received expected ->
        "Unexpected event: " <> show received <> " " <> show expected <> "."
    Yaml.InvalidYaml Nothing ->
        "Unknown YAML error."
    Yaml.InvalidYaml (Just (Yaml.YamlException yamlException)) ->
        "YAML error: " <> yamlException <> "."
    Yaml.InvalidYaml (Just (Yaml.YamlParseException prob ctx mark)) ->
        "YAML parse error: " <> prob <> " " <> ctx <> " " <> show mark <> "."
    Yaml.AesonException string ->
        "Aeson exception: " <> string <> "."
    Yaml.OtherParseException someException ->
        "Other parse exception: " <> show someException <> "."
    Yaml.NonStringKeyAlias anchorName value ->
        "Non-string key alias: " <> show anchorName <> " " <> show value <> "."
    Yaml.CyclicIncludes ->
        "Cyclic includes."
    where
        preamble = "An error occured while reading a YAML-encoded nomicon. "

-- Markov generator ------------------------------------------------------------

generate :: Nomicon -> IO Name
generate nomicon =
    withSystemRandom . asGenST . markovGenerate $ _nomiconMarkovMap nomicon

markovGenerate :: MarkovMap -> GenST s -> ST s Name
markovGenerate (MarkovMap context markovMap) gen =
    go $ Seq.replicate context MarkovInitial
    where
        go name = case Map.lookup predecessor markovMap of
            Just key -> do
                successor <- genFromTable key gen
                go $ name |> successor
            Nothing ->
                return . Name $ (\(MarkovMedial x) -> x) <$>
                    Seq.dropWhileR (== MarkovFinal) (Seq.drop context name)
            where
                predecessor = Seq.drop (Seq.length name - context) name
