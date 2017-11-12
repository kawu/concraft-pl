{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | DAG-aware morphosyntax data layer in Polish.


module NLP.Concraft.Polish.DAG.Morphosyntax
(
-- * Tag
  Tag

-- * Edge
-- , Edge (..)
, Word (..)
, Interp (..)
, voidInterp
, Space (..)
-- , select
-- , select'
, selectWMap
, selectAnno

-- * Sentence
, Sent
, SentO (..)
, restore
, withOrig

-- * Conversion
, packSent
, packSentO
-- , packSeg

-- -- ** From simple sentence
-- , fromList
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Data.Binary (Binary, put, get, putWord8, getWord8)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Tagset.Positional as P

import           Data.DAG (DAG)
import qualified Data.DAG as DAG
-- import           Data.CRF.Chain1.Constrained.DAG.Dataset.Internal (DAG)
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG

-- import qualified NLP.Concraft.DAG2 as C
import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.Polish.Morphosyntax as R
import           NLP.Concraft.Polish.Morphosyntax (Space(..))


--------------------------------
-- Basics
--------------------------------


-- | A textual representation of a morphosyntactic tag.
type Tag = T.Text


--------------------------------
-- Interp
--------------------------------


-- | A morphosyntactic interpretation.
data Interp t = Interp
    { base  :: T.Text
      -- ^ The base form (lemma)
    , tag   :: t
      -- ^ The (morphosyntactic) tag
    , commonness :: Maybe T.Text
    , qualifier  :: Maybe T.Text
    , metaInfo   :: Maybe T.Text
    , eos        :: Bool
      -- ^ The remaining four are ignored for the moment, but we plan to rely on
      -- them later on.
    } deriving (Show, Eq, Ord)


-- | An almost empty interpretation, with only the `tag` actually specified.
voidInterp :: t -> Interp t
voidInterp x = Interp
  { base = "none"
  , tag = x
  , commonness = Nothing
  , qualifier = Nothing
  , metaInfo = Nothing
  , eos = False
  }


instance (Ord t, Binary t) => Binary (Interp t) where
    put Interp{..} = do
      put base
      put tag
      put commonness
      put qualifier
      put metaInfo
      put eos
    get = Interp <$> get <*> get <*> get <*> get <*> get <*> get


--------------------------------
-- Edge
--------------------------------


-- -- | An edge consists of a word and a set of morphosyntactic interpretations.
-- data Edge t = Edge
--     { word      :: Word
--     -- | Interpretations of the word, each interpretation annotated
--     -- with a /disamb/ Boolean value (if 'True', the interpretation
--     -- is correct within the context).
--     , interps   :: X.WMap (Interp t) }
--     deriving (Show, Eq, Ord)
--
-- instance (Ord t, Binary t) => Binary (Edge t) where
--     put Edge{..} = put word >> put interps
--     get = Edge <$> get <*> get
--
-- instance X.Word (Edge t) where
--     orth = X.orth . word
--     oov = X.oov . word


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


---------------------------------------------------------------------------------
-- Selection
--
-- (Honestly, I don't remember what is this one about...)
--
-- Update: maybe related to the fact that base forms have to be handled somehow?
---------------------------------------------------------------------------------


-- -- | Select one chosen interpretation.
-- select :: Ord a => a -> Edge a -> Edge a
-- select = select' []
--
--
-- -- | Select multiple interpretations and one chosen interpretation.
-- select' :: Ord a => [a] -> a -> Edge a -> Edge a
-- select' ys x = selectWMap . X.mkWMap $ (x, 1) : map (,0) ys


-- | Select interpretations.
selectAnno :: Ord a => M.Map (Interp a) Double -> X.Seg Word (Interp a) -> X.Seg Word (Interp a)
selectAnno = selectWMap . X.fromMap


-- | Select interpretations.
selectWMap :: Ord a => X.WMap (Interp a) -> X.Seg Word (Interp a) -> X.Seg Word (Interp a)
selectWMap wMap seg = seg {X.tags = wMap}
--     seg { X.tags = newTags }
--   where
--     wSet = S.fromList . map tag . M.keys . X.unWMap . X.tags $ seg
--     newTags = X.mkWMap $
--         -- [ case M.lookup (tag interp) (X.unWMap wMap) of
--         [ case M.lookup interp (X.unWMap wMap) of
--             Just x  -> (interp, x)
--             Nothing -> (interp, 0)
--         | interp <- (M.keys . X.unWMap) (X.tags seg) ]
--             ++ catMaybes
--         [ if interp `S.member` wSet
--             then Nothing
--             else Just (interp, x)
--         | (interp, x) <- M.toList (X.unWMap wMap)
-- --         | let lemma = orth $ X.word seg   -- Default base form
-- --         , (tag, x) <- M.toList (X.unWMap wMap)
-- --         , let interp = Interp
-- --                 { base = lemma
-- --                 , tag = tag
-- --                 , commonness = Nothing
-- --                 , qualifier = Nothing
-- --                 , metaInfo = Nothing
-- --                 , eos = False }
--         ]


--------------------------------
-- Sentence
--------------------------------


-- | A sentence.
-- type Sent t = DAG Space (Edge t)
type Sent t = DAG Space (X.Seg Word t)


-- | A sentence with its original textual representation.
data SentO t = SentO
    { sent  :: Sent t
    , orig  :: L.Text }


-- | Restore textual representation of a sentence.
-- The function is not very accurate, it could be improved
-- if we enriched the representation of spaces.
restore :: Sent t -> L.Text
restore =
    let edgeStr = orth . X.word
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


-- -- | Convert a segment to a segment from the core library.
-- packSeg :: Ord a => X.Seg Word a -> X.Seg Word a
-- packSeg = id
-- -- packSeg Edge{..}
-- --     = X.Seg word
-- --     $ X.mkWMap
-- --     $ map (first tag)
-- --     $ M.toList
-- --     $ X.unWMap interps


-- | Convert a sentence to a sentence from the core library.
packSent :: Ord a => Sent a -> X.Sent Word a
packSent
  = DAG.mapN (const ())
  -- . fmap packSeg


-- | Convert a sentence to a sentence from the core library.
-- packSentO :: P.Tagset -> SentO Tag -> X.SentO Word P.Tag
packSentO :: Ord a => SentO a -> X.SentO Word a
packSentO s = X.SentO
    { segs = packSent (sent s)
    , orig = orig s }


---------------------------
-- From simple sentence
---------------------------


-- fromWord :: R.Word -> (Space, Word)
-- fromWord R.Word{..} = (space, Word {orth=orth, known=known})
--
--
-- fromSeg :: (Ord t) => R.Seg t -> (Space, Edge t)
-- fromSeg R.Seg{..} =
--   (space, edge)
--   where
--     edge = Edge {word = newWord, interps = updateInterps interps}
--     (space, newWord) = fromWord word
--     -- updateInterps = X.mkWMap . map (first fromInterp) . X.unWMap
--     updateInterps = X.mapWMap fromInterp
--
--
-- fromInterp :: R.Interp t -> Interp t
-- fromInterp R.Interp{..} = Interp
--   { base = base
--   , tag = tag
--   , commonness = Nothing
--   , qualifier = Nothing
--   , metaInfo = Nothing
--   , eos = False }
--
--
-- -- | Create a DAG-based sentence from a list-based sentence.
-- fromList :: Ord t => R.Sent t -> Sent t
-- fromList = DAG.fromList' None . map fromSeg
