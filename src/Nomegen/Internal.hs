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
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: $Header$
-- Description: Random name generation
-- Copyright: 2015 Ian D. Bollinger
-- License: MIT or Apache 2.0
-- Maintainer: Ian D. Bollinger <ian.bollinger@gmail.com>
-- Stability: unstable
-- Portability: portable
module Nomegen.Internal (
    -- * Nomicon
    Nomicon (..),

    -- * Name
    Name (..),
    nameToText,

    -- * Segment
    Segment (..),

    -- * Markov generator
    MarkovElement (..),
    MarkovMap (..),
    ) where

import Control.Monad (mzero)
import Data.Data (Data, Typeable)
import qualified Data.Foldable as Foldable
import Data.Monoid (Monoid, (<>))
import Data.String (IsString)
import GHC.Generics (Generic)
import GHC.Exts (IsList, Item, fromList, toList)

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, (.=), (.:), parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (
    Seq, ViewL ((:<), EmptyL), ViewR ((:>)), (<|), (|>), viewl, viewr,
    )
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import System.Random.MWC.CondensedTable (CondensedTableV, tableFromWeights)

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

parseNames :: Set Segment -> [Text] -> Seq Name
parseNames segments = Seq.fromList . fmap (parseName segments . Text.toLower)

parseName :: Set Segment -> Text -> Name
parseName segments = Name . Seq.unfoldr parse
    where
        parse entry
            | Text.null entry = Nothing
            | otherwise = Just (Segment segment, remainder)
                where
                    (segment, remainder) = prefix segments 0 entry

prefix :: Set Segment -> Int -> Text -> (Text, Text)
prefix segments n entry
    | Segment (Text.dropEnd n entry) `Set.member` segments =
        Text.splitAt (Text.length entry - n) entry
    | n < Text.length entry = prefix segments (n + 1) entry
    | otherwise = error $ "Unknown segment " <> Text.unpack entry

buildMarkovMap :: Int -> Seq Name -> MarkovMap
buildMarkovMap context =
    MarkovMap context . fmap buildCondensedTableV . countSegments context

-- | Build a 'CondensedTableV' from a 'Map' of weights.
buildCondensedTableV :: Map a Double -> CondensedTableV a
buildCondensedTableV = tableFromWeights . Vector.fromList . Map.toAscList

data MarkovElement
    = MarkovInitial
    | MarkovMedial !Segment
    | MarkovFinal
    deriving (Eq, Ord)

type Weights = Map MarkovElement Double
type WeightMap = Map (Seq MarkovElement) Weights

countSegments :: Int -> Seq Name -> WeightMap
countSegments context = Foldable.foldl' doNames Map.empty
    where
        doNames :: WeightMap -> Name -> WeightMap
        doNames weights name =
            Foldable.foldl' doSegments weights
            . windows (context + 1)
            $ Seq.replicate context MarkovInitial <>
                fmap MarkovMedial (_nameSegments name) |> MarkovFinal
        doSegments :: WeightMap -> Seq MarkovElement -> WeightMap
        doSegments weightMap window =
            Map.alter f predecessor weightMap
            where
                f :: Maybe Weights -> Maybe Weights
                f = Just . \case
                    Just x -> Map.insertWith (+) successor 1.0 x
                    Nothing -> Map.singleton successor 1.0
                predecessor :> successor = viewr window

-- | Sliding windows over a sequence. That is, @windows n xs@ is every
-- length-/n/ subsequence of /xs/ in order.
windows :: Int -> Seq a -> Seq (Seq a)
windows n0 = go 0 Seq.empty
    where
        go n s ax = case viewl ax of
            EmptyL -> Seq.empty
            a :< as
                | n' < n0 -> go n' s' as
                | n' == n0 -> s' <| go n' s' as
                | otherwise -> s'' <| go n s'' as
                where
                    n' = n + 1
                    s' = s |> a
                    (_ :< s'') = viewl s'

instance ToJSON Nomicon where
    toJSON Nomicon {..} = Json.object [
        "segments" .= _nomiconSegments,
        "entries" .= Foldable.toList _nomiconEntries
        ]

-- Name ------------------------------------------------------------------------

newtype Name = Name { _nameSegments :: Seq Segment }
    deriving (
        Data,
        Eq,
        Ord,
        Read,
        Show,
        Generic,
        Monoid,
        NFData,
        Typeable
        )

instance IsList Name where
    type Item Name = Segment
    fromList = Name . Seq.fromList
    toList = Foldable.toList . _nameSegments

instance ToJSON Name where
    toJSON = Json.String . nameToText

nameToText :: Name -> Text
nameToText = Text.concat . Foldable.toList . fmap _segmentText . _nameSegments

-- Segment ---------------------------------------------------------------------

newtype Segment = Segment { _segmentText :: Text }
    deriving (
        Data,
        Eq,
        Ord,
        Read,
        Show,
        Generic,
        Hashable,
        IsString,
        Monoid,
        NFData,
        Typeable
        )

instance IsList Segment where
    type Item Segment = Char
    fromList = Segment . fromList
    toList = toList . _segmentText

instance FromJSON Segment where
    parseJSON = \case
        Json.String x -> return $ Segment x
        _ -> mzero

instance ToJSON Segment where
    toJSON = Json.String . _segmentText

-- Markov generator ------------------------------------------------------------

data MarkovMap =
    MarkovMap !Int !(Map (Seq MarkovElement) (CondensedTableV MarkovElement))
