{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft.Polish
(
-- * Model
  C.Concraft
, C.saveModel
, C.loadModel

-- * Tagging
, tag
, tag'
, tagSent

-- * Training
, TrainConf (..)
, train
) where


import qualified Control.Monad.LazyIO as LazyIO
import           Control.Applicative ((<$>))
import qualified Data.List.Split as Split
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD as SGD

import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Schema as S
import           NLP.Concraft.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D
import qualified NLP.Concraft as C

import           NLP.Concraft.Polish.Morphosyntax hiding (tag)
import           NLP.Concraft.Polish.Maca


-------------------------------------------------
-- Default configuration
-------------------------------------------------


-- | Default configuration for the guessing observation schema.
guessSchemaDefault :: SchemaConf
guessSchemaDefault = S.nullConf
    { lowPrefixesC  = entryWith [1, 2]      [0]
    , lowSuffixesC  = entryWith [1, 2]      [0]
    , knownC        = entry                 [0]
    , begPackedC    = entry                 [0] }


-- | Default configuration for the guessing observation schema.
disambSchemaDefault :: SchemaConf
disambSchemaDefault = S.nullConf
    { lowOrthC      = entry                         [-1, 0, 1]
    , lowPrefixesC  = oov $ entryWith [1, 2, 3]     [0]
    , lowSuffixesC  = oov $ entryWith [1, 2, 3]     [0]
    , begPackedC    = oov $ entry                   [0] }
  where
    oov (Just body) = Just $ body { S.oovOnly = True }
    oov Nothing     = Nothing


-- | Default tiered tagging configuration.
tiersDefault :: [D.Tier]
tiersDefault =
    [tier1, tier2]
  where
    tier1 = D.Tier True $ S.fromList ["cas", "per"]
    tier2 = D.Tier False $ S.fromList
        [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
        , "acn", "ppr", "agg", "vlc", "dot" ]


-------------------------------------------------
-- Tagging
-------------------------------------------------


-- | Perform morphological tagging on the input text.
tag :: MacaPool -> C.Concraft -> T.Text -> IO [Sent Tag]
tag pool concraft inp = map (tagSent concraft) <$> macaPar pool inp


-- | An alernative tagging function which interprets
-- empty lines as paragraph ending markers.
-- The function uses lazy IO so it can be used to
-- analyse large chunks of data.
tag' :: MacaPool -> C.Concraft -> L.Text -> IO [[Sent Tag]]
tag' pool concraft
    = LazyIO.mapM (tag pool concraft . L.toStrict)
    . map L.unlines
    . Split.splitWhen
        (L.all Char.isSpace)
    . L.lines


-- | Tag an already analysed sentence.
tagSent :: C.Concraft -> Sent Tag -> Sent Tag
tagSent concraft sent =
    let tagset = C.tagset concraft
        packed = packSent tagset sent
        tags   = map (P.showTag tagset) (C.tag concraft packed)
    in  map (uncurry select) (zip tags sent)


-------------------------------------------------
-- Training
-------------------------------------------------


data TrainConf = TrainConf {
    -- | Tagset.
      tagset    :: P.Tagset
    -- | SGD parameters.
    , sgdArgs   :: SGD.SgdArgs
    -- | Perform reanalysis.
    , reana     :: Bool
    -- | Store SGD dataset on disk.
    , onDisk    :: Bool
    -- | Numer of guessed tags for each word.
    , guessNum  :: Int
    -- | Disamb model pruning parameter.
    , prune     :: Maybe Double
    -- | `G.r0T` parameter.
    , r0        :: G.R0T }

-- | Train concraft model.
-- TODO: It should be possible to supply the two training procedures with
-- different SGD arguments.
train
    :: TrainConf
    -> IO [SentO Tag]      -- ^ Training data
    -> IO [SentO Tag]      -- ^ Evaluation data
    -> IO C.Concraft
train TrainConf{..} train0 eval0 = do

    pool <- newMacaPool 1
    let ana = fmap (packSent tagset . concat) . macaPar pool . L.toStrict
        train1 = map (packSentO tagset) <$> train0
        eval1  = map (packSentO tagset) <$> eval0

    if reana
        then doReana ana train1 eval1
        else noReana     train1 eval1

  where

    guessConf  = G.TrainConf guessSchemaDefault sgdArgs onDisk r0
    disambConf = D.TrainConf tiersDefault disambSchemaDefault
        sgdArgs onDisk prune

    doReana ana   = C.reAnaTrain tagset ana guessNum guessConf disambConf
    noReana tr ev = C.train tagset guessNum guessConf disambConf 
        (map X.segs <$> tr) (map X.segs <$> ev)
