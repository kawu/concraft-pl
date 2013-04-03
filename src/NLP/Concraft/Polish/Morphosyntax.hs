{-# LANGUAGE RecordWildCards #-}

-- | Morphosyntax data layer in Polish.

module NLP.Concraft.Polish.Morphosyntax
( 
-- * Segment 
  Seg (..)
, Word (..)
, Interp (..)
, Space (..)
, Tag

-- * Sentence
, Sent

-- * Conversion
, packSegTag
, packSeg
, packSentTag
, packSent
, embedSent
) where

import           Control.Arrow (first)
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tagset.Positional as P

import qualified NLP.Concraft.Morphosyntax as X

-- | A segment.
data Seg t = Seg 
    { word      :: Word
    -- | Interpretations of the token, each interpretation annotated
    -- with a /disamb/ Boolean value (if 'True', the interpretation
    -- is correct within the context).
    , interps   :: M.Map (Interp t) Bool }
    deriving (Show, Eq, Ord)

-- | A word.
data Word = Word
    { orth      :: T.Text
    , space     :: Space
    , known     :: Bool }
    deriving (Show, Eq, Ord)

instance X.HasOrth Word where
    orth = orth

instance X.HasOOV Word where
    oov = not.known
    
-- | An interpretation.
-- TODO: Should we allow `base` to be `Nothing`?
data Interp t = Interp
    { base  :: Maybe T.Text
    , tag   :: t }
    deriving (Show, Eq, Ord)

-- | No space, space or newline.
-- TODO: Perhaps we should use a bit more informative data type.
data Space
    = None
    | Space
    | NewLine
    deriving (Show, Eq, Ord)

-- | A sentence.
type Sent t = [Seg t]

-- | A textual representation of a morphosyntactic tag.
type Tag = T.Text

---------------------------
-- Conversion
---------------------------

-- | Convert a segment to a segment from a core library.
packSegTag :: P.Tagset -> Seg Tag -> X.Seg Word P.Tag
packSegTag tagset = X.mapSeg (P.parseTag tagset) . packSeg

-- | Convert a segment to a segment from a core library.
packSeg :: Ord a => Seg a -> X.Seg Word a
packSeg Seg{..} = X.Seg word $ X.mkWMap
    [ (tag x, if disamb then 1 else 0)
    | (x, disamb) <- M.toList interps ]

-- | Convert a sentence to a sentence from a core library.
packSentTag :: P.Tagset -> Sent Tag -> X.Sent Word P.Tag
packSentTag = map . packSegTag

-- | Convert a sentence to a sentence from a core library.
packSent :: Ord a => Sent a -> X.Sent Word a
packSent = map packSeg

-- | Embed tags in sentence.
embedSent :: Ord a => Sent a -> [a] -> Sent a
embedSent sent xs = [selectOne x seg | (x, seg) <- zip xs sent]

-- | Select one interpretation.
selectOne :: Ord a => a -> Seg a -> Seg a
selectOne x = select (X.mkWMap [(x, 1)])

-- | Select interpretations.
select :: Ord a => X.WMap a -> Seg a -> Seg a
select wMap seg =
    seg { interps = newInterps }
  where
    wSet = M.fromList . map (first tag) . M.toList . interps
    asDmb x = if x > 0
        then True
        else False
    newInterps = M.fromList $
        [ case M.lookup (tag interp) (X.unWMap wMap) of
            Just x  -> (interp, asDmb x)
            Nothing -> (interp, False)
        | interp <- M.keys (interps seg) ]
            ++ catMaybes
        [ if tag `M.member` wSet seg
            then Nothing
            else Just (Interp Nothing tag, asDmb x)
        | (tag, x) <- M.toList (X.unWMap wMap) ]
