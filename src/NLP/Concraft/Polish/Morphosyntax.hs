{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | Morphosyntax data layer in Polish.


module NLP.Concraft.Polish.Morphosyntax
( 
-- * Tag
  Tag

-- * Segment 
, Seg (..)
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
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Data.Maybe (catMaybes)
import           Data.Aeson
import           Data.Binary (Binary, put, get, putWord8, getWord8)
import qualified Data.Aeson as Aeson
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Tagset.Positional as P

import qualified NLP.Concraft.Morphosyntax as X


-- | A textual representation of a morphosyntactic tag.
type Tag = T.Text


--------------------------------
-- Segment
--------------------------------


-- | A segment consists of a word and a set of morphosyntactic interpretations.
data Seg t = Seg 
    { word      :: Word
    -- | Interpretations of the token, each interpretation annotated
    -- with a /disamb/ Boolean value (if 'True', the interpretation
    -- is correct within the context).
    , interps   :: X.WMap (Interp t) }
    deriving (Show, Eq, Ord)


instance (Ord t, Binary t) => Binary (Seg t) where
    put Seg{..} = put word >> put interps
    get = Seg <$> get <*> get
    

-- | A word.
data Word = Word
    { orth      :: T.Text
    , space     :: Space
    , known     :: Bool }
    deriving (Show, Eq, Ord)


instance X.Word Word where
    orth = orth
    oov = not.known


instance ToJSON Word where
    toJSON Word{..} = object
        [ "orth"  .= orth
        , "space" .= space
        , "known" .= known ]


instance FromJSON Word where
    parseJSON (Object v) = Word
        <$> v .: "orth"
        <*> v .: "space"
        <*> v .: "known"
    parseJSON _ = error "parseJSON [Word]"


instance Binary Word where
    put Word{..} = put orth >> put space >> put known
    get = Word <$> get <*> get <*> get
    

-- | A morphosyntactic interpretation.
data Interp t = Interp
    { base  :: T.Text
    , tag   :: t }
    deriving (Show, Eq, Ord)


instance (Ord t, Binary t) => Binary (Interp t) where
    put Interp{..} = put base >> put tag
    get = Interp <$> get <*> get


-- | No space, space or newline.
-- TODO: Perhaps we should use a bit more informative data type.
data Space
    = None
    | Space
    | NewLine
    deriving (Show, Eq, Ord)


instance Binary Space where
    put x = case x of
        None    -> putWord8 1
        Space   -> putWord8 2
        NewLine -> putWord8 3
    get = getWord8 >>= \x -> return $ case x of
        1   -> None
        2   -> Space
        _   -> NewLine


instance ToJSON Space where
    toJSON x = Aeson.String $ case x of
        None    -> "none"
        Space   -> "space"
        NewLine -> "newline"


instance FromJSON Space where
    parseJSON (Aeson.String x) = return $ case x of
        "none"      -> None
        "space"     -> Space
        "newline"   -> NewLine
        _           -> error "parseJSON [Space]"
    parseJSON _ = error "parseJSON [Space]"


-- | Select one chosen interpretation.
select :: Ord a => a -> Seg a -> Seg a
select = select' []


-- | Select multiple interpretations and one chosen interpretation.
select' :: Ord a => [a] -> a -> Seg a -> Seg a
select' ys x = selectWMap . X.mkWMap $ (x, 1) : map (,0) ys


-- | Select interpretations.
selectWMap :: Ord a => X.WMap a -> Seg a -> Seg a
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
type Sent t = [Seg t]


-- | A sentence.
data SentO t = SentO
    { segs  :: [Seg t]
    , orig  :: L.Text }


-- | Restore textual representation of a sentence.
-- The function is not very accurate, it could be improved
-- if we enrich representation of a space.
restore :: Sent t -> L.Text
restore =
    let wordStr Word{..} = [spaceStr space, orth] 
        spaceStr None       = ""
        spaceStr Space      = " "
        spaceStr NewLine    = "\n"
    in  L.fromChunks . concatMap (wordStr . word)


-- | Use `restore` to translate `Sent` to a `SentO`.
withOrig :: Sent t -> SentO t
withOrig s = SentO
    { segs = s
    , orig = restore s }


---------------------------
-- Conversion
---------------------------


-- | Convert a segment to a segment from a core library.
packSeg_ :: Ord a => Seg a -> X.Seg Word a
packSeg_ Seg{..}
    = X.Seg word
    $ X.mkWMap
    $ map (first tag)
    $ M.toList
    $ X.unWMap interps


-- | Convert a segment to a segment from a core library.
packSeg :: P.Tagset -> Seg Tag -> X.Seg Word P.Tag
packSeg tagset = X.mapSeg (P.parseTag tagset) . packSeg_


-- | Convert a sentence to a sentence from a core library.
packSent :: P.Tagset -> Sent Tag -> X.Sent Word P.Tag
packSent = map . packSeg


-- | Convert a sentence to a sentence from a core library.
packSentO :: P.Tagset -> SentO Tag -> X.SentO Word P.Tag
packSentO tagset s = X.SentO
    { segs = packSent tagset (segs s)
    , orig = orig s }
