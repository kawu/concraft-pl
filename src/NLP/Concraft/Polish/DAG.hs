{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | DAG-based model for morphosyntactic tagging.
-- For the moment based only on the guessing model.


module NLP.Concraft.Polish.DAG
(
-- * Model
  C.Concraft
, C.saveModel
, C.loadModel

-- * Tagging
, marginals
-- , tag

-- * Trimming
, trimOOV

-- * Training
, TrainConf (..)
, train

-- -- * Pruning
-- , C.prune
) where


import           Control.Applicative ((<$>))
import qualified Data.Text.Lazy as L
import qualified Data.Set as S

import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD as SGD

import qualified Data.DAG as DAG

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Schema as S
import           NLP.Concraft.DAG.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.DAG.Guess as G
import qualified NLP.Concraft.DAG as C


import           NLP.Concraft.Polish.DAG.Morphosyntax hiding (tag)
-- import           NLP.Concraft.Polish.Maca


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


-------------------------------------------------
-- Tagging
-------------------------------------------------


-- | Tag the sentence with marginal probabilities.
marginals :: C.Concraft -> Sent Tag -> Sent Tag
marginals concraft sent
    = fmap select
    $ DAG.zipE sent wmaps
  where
    select (edge, wmap) = selectWMap wmap edge
    tagset = C.tagset concraft
    packed = packSent tagset sent
    wmaps  = fmap
        (X.mapWMap showTag)
        (C.marginals concraft packed)
    showTag = P.showTag tagset


-------------------------------------------------
-- Trimming
-------------------------------------------------


-- | Trim down the set of potential labels to `k` most probable ones
-- for each OOV word in the sentence.
trimOOV :: Int -> Sent Tag -> Sent Tag
trimOOV k =
  fmap trim
  where
    trim edge = if X.oov edge
      then trimEdge edge
      else edge
    trimEdge edge = edge {interps = X.trim k (interps edge)}

-------------------------------------------------
-- Training
-------------------------------------------------


-- | Training configuration.
data TrainConf = TrainConf {
    -- | Tagset.
      tagset    :: P.Tagset
    -- | SGD parameters.
    , sgdArgs   :: SGD.SgdArgs
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
    -> IO [Sent Tag]      -- ^ Training data
    -> IO [Sent Tag]      -- ^ Evaluation data
    -> IO C.Concraft
train TrainConf{..} train0 eval0 = do
  let train1 = map (packSent tagset) <$> train0
      eval1  = map (packSent tagset) <$> eval0
  noReana train1 eval1
  where
    noReana tr ev = C.train tagset guessNum guessConf tr ev
        -- (map X.segs <$> tr) (map X.segs <$> ev)
    guessConf  = G.TrainConf guessSchemaDefault sgdArgs onDisk r0
