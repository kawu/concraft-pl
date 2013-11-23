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
, marginals

-- * Analysis
, macaPar

-- * Training
, TrainConf (..)
, train

-- * Pruning
, C.prune

-- -- * Analysis
-- , anaSent
-- , reAnaPar
) where


import           Control.Applicative ((<$>))
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
-- import qualified NLP.Concraft.Analysis as A

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
    { lowOrthC      = entry                         [-2, -1, 0, 1]
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


-- | Tag the analysed sentence.
tag :: C.Concraft -> Sent Tag -> Sent Tag
tag concraft sent =
    [ select' gs t seg
    | (seg, gs, t) <- zip3 sent gss ts ]
  where
    tagset = C.tagset concraft
    packed = packSent tagset sent
    tagged = C.tag concraft packed
    gss    = map (map showTag . S.toList . fst) tagged
    ts     = map (showTag . snd) tagged
    showTag = P.showTag tagset


-- | Tag the sentence with marginal probabilities.
marginals :: C.Concraft -> Sent Tag -> Sent Tag
marginals concraft sent
    = map (uncurry selectWMap)
    $ zip wmaps sent
  where
    tagset = C.tagset concraft
    packed = packSent tagset sent
    wmaps  = map
        (X.mapWMap showTag)
        (C.marginals concraft packed)
    showTag = P.showTag tagset


-------------------------------------------------
-- Training
-------------------------------------------------


-- | Training configuration.
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
    let ana = anaSent tagset pool
        train1 = map (packSentO tagset) <$> train0
        eval1  = map (packSentO tagset) <$> eval0

    if reana
        then doReana ana train1 eval1
        else noReana     train1 eval1

  where

    doReana ana   = C.reAnaTrain tagset ana guessNum guessConf disambConf
    noReana tr ev = C.train tagset guessNum guessConf disambConf 
        (map X.segs <$> tr) (map X.segs <$> ev)

    guessConf  = G.TrainConf guessSchemaDefault sgdArgs onDisk r0
    disambConf = D.TrainConf tiersDefault disambSchemaDefault sgdArgs onDisk


-------------------------------------------------
-- Re-analysis
-------------------------------------------------


-- | Analyse the given sentence with Maca.
-- anaSent :: MacaPool -> L.Text -> IO (Sent Tag)
anaSent :: P.Tagset -> MacaPool -> L.Text -> IO (X.Sent Word P.Tag)
anaSent tagset pool
    = fmap (packSent tagset . concat)
    . macaPar pool . L.toStrict


-- -- | Reanalyse the input paragraph (lazy IO).
-- reAnaPar :: P.Tagset -> [SentO Tag] -> IO [Sent Tag]
-- reAnaPar tagset inp = do
--     pool <- newMacaPool 1
--     A.reAnaPar tagset (anaSent pool) inp
