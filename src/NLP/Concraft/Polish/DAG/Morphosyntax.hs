{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | DAG-aware morphosyntax data layer in Polish.


module NLP.Concraft.Polish.DAG.Morphosyntax
(
-- * Tag
  Tag

-- * Edge
, Edge (..)
, Word (..)
, Interp (..)
, Space (..)
, select
, select'
, selectWMap

-- * Sentence
, Sent
, SentO (..)
, restore
, withOrig

-- * Conversion
, packSeg
, packSent
, packSentO

, fromList
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Data.Binary (Binary, put, get, putWord8, getWord8)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Tagset.Positional as P

import           Data.DAG (DAG)
import qualified Data.DAG as DAG
-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.Polish.Morphosyntax as R
import           NLP.Concraft.Polish.Morphosyntax (Space(..), Interp(..))


-- | A textual representation of a morphosyntactic tag.
type Tag = T.Text


--------------------------------
-- Edge
--------------------------------


-- | An edge consists of a word and a set of morphosyntactic interpretations.
data Edge t = Edge
    { word      :: Word
    -- | Interpretations of the word, each interpretation annotated
    -- with a /disamb/ Boolean value (if 'True', the interpretation
    -- is correct within the context).
    , interps   :: X.WMap (Interp t) }
    deriving (Show, Eq, Ord)


instance (Ord t, Binary t) => Binary (Edge t) where
    put Edge{..} = put word >> put interps
    get = Edge <$> get <*> get


--------------------------------
-- Word
--------------------------------


-- | A word.
data Word = Word
    { orth      :: T.Text
    -- , space     :: Space
    , known     :: Bool }
    deriving (Show, Eq, Ord)

instance X.Word Word where
    orth = orth
    oov = not.known

instance Binary Word where
    -- put Word{..} = put orth >> put space >> put known
    put Word{..} = put orth >> put known
    -- get = Word <$> get <*> get <*> get
    get = Word <$> get <*> get

instance ToJSON Word where
    toJSON Word{..} = object
        [ "orth"  .= orth
        , "known" .= known ]

instance FromJSON Word where
    parseJSON (Object v) = Word
        <$> v .: "orth"
        <*> v .: "known"
    parseJSON _ = error "parseJSON [Word]"


--------------------------------
-- Selection
--
-- (Honestly, I don't remember
-- what is this one about...)
--------------------------------


-- | Select one chosen interpretation.
select :: Ord a => a -> Edge a -> Edge a
select = select' []


-- | Select multiple interpretations and one chosen interpretation.
select' :: Ord a => [a] -> a -> Edge a -> Edge a
select' ys x = selectWMap . X.mkWMap $ (x, 1) : map (,0) ys


-- | Select interpretations.
selectWMap :: Ord a => X.WMap a -> Edge a -> Edge a
selectWMap wMap seg =
    seg { interps = newInterps }
  where
    wSet = S.fromList . map tag . M.keys . X.unWMap . interps $ seg
    newInterps = X.mkWMap $
        [ case M.lookup (tag interp) (X.unWMap wMap) of
            Just x  -> (interp, x)
            Nothing -> (interp, 0)
        | interp <- (M.keys . X.unWMap) (interps seg) ]
            ++ catMaybes
        [ if tag `S.member` wSet
            then Nothing
            else Just (Interp lemma tag, x)
        | let lemma = orth $ word seg   -- Default base form
        , (tag, x) <- M.toList (X.unWMap wMap) ]


--------------------------------
-- Sentence
--------------------------------


-- | A sentence.
type Sent t = DAG Space (Edge t)


-- | A sentence with its original textual representation.
data SentO t = SentO
    { sent  :: Sent t
    , orig  :: L.Text }


-- | Restore textual representation of a sentence.
-- The function is not very accurate, it could be improved
-- if we enriched the representation of spaces.
restore :: Sent t -> L.Text
restore =
    let edgeStr = orth . word
        spaceStr None    = ""
        spaceStr Space   = " "
        spaceStr NewLine = "\n"
    in  L.fromChunks . map (either spaceStr edgeStr) . pickPath


-- | Use `restore` to translate `Sent` to a `SentO`.
withOrig :: Sent t -> SentO t
withOrig s = SentO
    { sent = s
    , orig = restore s }


--------------------------------
-- Utils
--------------------------------


-- | Pick any path from the given DAG. The result is a list
-- of interleaved node and edge labels.
pickPath :: DAG a b -> [Either a b]
pickPath = undefined


---------------------------
-- Conversion
---------------------------


-- | Convert a segment to a segment from the core library.
packSeg_ :: Ord a => Edge a -> X.Seg Word a
packSeg_ Edge{..}
    = X.Seg word
    $ X.mkWMap
    $ map (first tag)
    $ M.toList
    $ X.unWMap interps


-- | Convert a segment to a segment from the core library.
packSeg :: P.Tagset -> Edge Tag -> X.Seg Word P.Tag
packSeg tagset = X.mapSeg (P.parseTag tagset) . packSeg_


-- | Convert a sentence to a sentence from the core library.
packSent :: P.Tagset -> Sent Tag -> X.Sent Word P.Tag
packSent tagset
  = DAG.mapN (const ())
  . fmap (packSeg tagset)


-- | Convert a sentence to a sentence from the core library.
packSentO :: P.Tagset -> SentO Tag -> X.SentO Word P.Tag
packSentO tagset s = X.SentO
    { segs = packSent tagset (sent s)
    , orig = orig s }


---------------------------
-- From simple sentence
---------------------------


fromWord :: R.Word -> (Space, Word)
fromWord R.Word{..} = (space, Word {orth=orth, known=known})


fromSeg :: R.Seg t -> (Space, Edge t)
fromSeg R.Seg{..} =
  (space, edge)
  where
    edge = Edge {word = newWord , interps = interps}
    (space, newWord) = fromWord word


-- | Create a DAG-based sentence from a list-based sentence.
fromList :: R.Sent t -> Sent t
fromList = DAG.fromList' None . map fromSeg
