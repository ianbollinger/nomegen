-- Copyright 2015 Ian D. Bollinger
--
-- Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
-- http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
-- or http://opensource.org/licenses/MIT>, at your option. This file may not be
-- copied, modified, or distributed except according to those terms.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Control.Monad (liftM, mzero)
import Control.Monad.ST.Safe (ST)
import Data.Data (Data, Typeable)
import qualified Data.Foldable as Foldable
import Data.Monoid (Monoid, (<>), mempty)
import Data.Ord (comparing)
import Data.String (IsString)
import GHC.Generics (Generic)
import GHC.Exts (IsList, Item, fromList, toList)

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Lens.Cons (
    Cons, Snoc, _Cons, _Snoc, (<|), (|>), _tail, uncons, unsnoc,
    )
import Control.Lens.Each (Each, each)
import Control.Lens.Empty (AsEmpty)
import Control.Lens.Iso (Iso', iso)
import Data.Aeson (FromJSON, ToJSON, (.=), (.:), parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Bytes.Serial (Serial)
import Data.Default (Default, def)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MonoTraversable (
    Element, MonoFoldable, MonoFunctor, MonoPointed, MonoTraversable, otraverse,
    omapM,
    )
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import System.Random.MWC (GenST, asGenST, uniformR, withSystemRandom)
import System.Random.MWC.CondensedTable (
    CondensedTableV, genFromTable, tableFromWeights,
    )

-- Nomicon ---------------------------------------------------------------------

-- | A collection of 'Name' objects.
data Nomicon = Nomicon {
    _nomiconSegments :: !(Set Segment),
    _nomiconMarkovMap :: !MarkovMap,
    _nomiconEntries :: !(Seq Name)
    }

instance FromJSON Nomicon where
    parseJSON = \case
        Json.Object o -> do
            _nomiconSegments <- o .: "segments"
            entries <- o .: "entries"
            let _nomiconEntries = parseNames _nomiconSegments entries
                _nomiconMarkovMap = buildMarkovMap 2 _nomiconEntries
            return Nomicon {..}
        _ -> mzero

instance ToJSON Nomicon where
    toJSON Nomicon {..} = Json.object [
        "segments" .= _nomiconSegments,
        "entries" .= Foldable.toList _nomiconEntries
        ]

parseNames :: Set Segment -> [Text] -> Seq Name
parseNames segments = Seq.fromList . fmap (parseName segments . Text.toLower)

parseName :: Set Segment -> Text -> Name
parseName segments = Name . Seq.unfoldr parse
    where
        parse entry
            | Text.null entry = Nothing
            | otherwise = Just (makeSegment segment, remainder)
                where
                    (segment, remainder) = prefix segments 0 entry

prefix :: Set Segment -> Int -> Text -> (Text, Text)
prefix segments n entry
    | makeSegment (Text.dropEnd n entry) `Set.member` segments =
        Text.splitAt (Text.length entry - n) entry
    | n < Text.length entry = prefix segments (n + 1) entry
    | otherwise = error $ "Unknown segment " <> Text.unpack entry

buildMarkovMap :: Int -> Seq Name -> MarkovMap
buildMarkovMap context =
    MarkovMap context . fmap buildCondensedTableV . countSegments context

-- | Build a 'CondensedTableV' from a 'Map' of weights.
buildCondensedTableV :: Map a Double -> CondensedTableV a
buildCondensedTableV = tableFromWeights . Vector.fromList . Map.toAscList

type Weights = Map Segment Double
type WeightMap = Map (Seq Segment) Weights

countSegments :: Int -> Seq Name -> WeightMap
countSegments context = Foldable.foldl' doNames mempty
    where
        doNames :: WeightMap -> Name -> WeightMap
        doNames weights =
            Foldable.foldl' doSegments weights
            . windows (context + 1)
            . nameSegments
        doSegments :: WeightMap -> Seq Segment -> WeightMap
        doSegments weightMap window =
            Map.alter f predecessor weightMap
            where
                f :: Maybe Weights -> Maybe Weights
                f = Just . \case
                    Just x -> Map.insertWith (+) successor 1.0 x
                    Nothing -> Map.singleton successor 1.0
                Just (predecessor, successor) = unsnoc window

-- | Sliding windows over a sequence. That is, @windows n xs@ is every
-- length-/n/ subsequence of /xs/ in order.
windows :: Int -> Seq a -> Seq (Seq a)
windows n0 = go 0 mempty
    where
        go n s ax = case uncons ax of
            Nothing -> mempty
            Just (a, as)
                | n' < n0 -> go n' s' as
                | n' == n0 -> s' <| go n' s' as
                | otherwise -> s'' <| go n s'' as
                where
                    n' = n + 1
                    s' = s |> a
                    s'' = s' ^. _tail

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

newtype Name = Name (Seq Segment)
    deriving (
        Data,
        Default,
        Eq,
        Ord,
        Read,
        Show,
        Generic,
        MonoFunctor,
        Monoid,
        MonoPointed,
        NFData,
        Semigroup,
        Typeable
        )

type instance Element Name = Segment

instance AsEmpty Name

instance Cons Name Name Segment Segment where
    _Cons = nameIso
        . _Cons
        . iso (second makeName) (second nameSegments)

instance Each Name Name Segment Segment where
    each = nameIso . each

instance IsList Name where
    type Item Name = Segment
    fromList = makeName . Seq.fromList
    toList = Foldable.toList . nameSegments

deriving instance MonoFoldable Name

instance MonoTraversable Name where
    otraverse f = fmap Name . otraverse f . nameSegments
    omapM f = liftM Name . omapM f . nameSegments

instance Serial Name

instance Snoc Name Name Segment Segment where
    _Snoc = nameIso
        . _Snoc
        . iso (first makeName) (first nameSegments)

instance ToJSON Name where
    toJSON = Json.String . nameToText

makeName :: Seq Segment -> Name
makeName = Name

nameSegments :: Name -> Seq Segment
nameSegments (Name x) = x

nameIso :: Iso' Name (Seq Segment)
nameIso = iso nameSegments makeName

nameToText :: Name -> Text
nameToText = Text.concat . Foldable.toList . fmap segmentToText . nameSegments

-- Segment ---------------------------------------------------------------------

newtype Segment = Segment Text
    deriving (
        Data,
        Eq,
        Ord,
        Read,
        Show,
        Generic,
        Hashable,
        IsString,
        MonoFunctor,
        Monoid,
        MonoPointed,
        NFData,
        Semigroup,
        Typeable
        )

type instance Element Segment = Char

instance AsEmpty Segment

instance Cons Segment Segment Char Char where
    _Cons = segmentIso
        . _Cons
        . iso (second makeSegment) (second segmentToText)

instance Default Segment where
    def = mempty

instance Each Segment Segment Char Char where
    each = segmentIso . each

instance IsList Segment where
    type Item Segment = Char
    fromList = makeSegment . fromList
    toList = toList . segmentToText

instance FromJSON Segment where
    parseJSON = \case
        Json.String x -> return $ makeSegment x
        _ -> mzero

deriving instance MonoFoldable Segment

instance MonoTraversable Segment where
    otraverse f = fmap Segment . otraverse f . segmentToText
    omapM f = liftM Segment . omapM f . segmentToText

instance Serial Segment

instance Snoc Segment Segment Char Char where
    _Snoc = segmentIso
        . _Snoc
        . iso (first makeSegment) (first segmentToText)

instance ToJSON Segment where
    toJSON = Json.String . segmentToText

segmentIso :: Iso' Segment Text
segmentIso = iso segmentToText makeSegment

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

data MarkovMap = MarkovMap !Int !(Map (Seq Segment) (CondensedTableV Segment))

generate :: Int -> Nomicon -> IO Name
generate iterations nomicon =
    withSystemRandom . asGenST $
        markovGenerate iterations (_nomiconMarkovMap nomicon)

markovGenerate :: Int -> MarkovMap -> GenST s -> ST s Name
markovGenerate iterations (MarkovMap context markovMap) gen =
    chooseKey markovMap gen >>= go iterations
    where
        go iterations' name
            | iterations' <= 0 = return $ Name name
            | otherwise =
                case Map.lookup predecessor markovMap of
                    Just key -> do
                        successor <- genFromTable key gen
                        go (iterations' - 1) (name |> successor)
                    Nothing -> return $ Name name
            where
                predecessor = Seq.drop (Seq.length name - context) name

-- | Randomly choose a key from the given 'Map'.
chooseKey :: Map k v -> GenST s -> ST s k
chooseKey map' gen =
    (keys !!) <$> uniformR (0, length keys - 1) gen
    where
        keys = Map.keys map'

