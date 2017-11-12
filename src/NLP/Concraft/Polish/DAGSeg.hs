{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | DAG-based model for morphosyntactic tagging.


module NLP.Concraft.Polish.DAGSeg
(
-- * Model
  C.Concraft
, C.saveModel
, C.loadModel

-- * Tagging
, guess
-- , disamb
-- , disamb'
-- , tag
-- , tag'
-- ** High level
, AnnoSent (..)
, annoAll

-- * Training
, TrainConf (..)
, train

-- * Pruning
-- , C.prune
) where


import           Control.Applicative ((<$>))
import           Control.Arrow (first)
import           Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD as SGD

import qualified Data.DAG as DAG

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Schema as S
import           NLP.Concraft.DAG.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.DAG.Guess as G
import qualified NLP.Concraft.DAG.DisambSeg as D
import qualified NLP.Concraft.DAGSeg as C

import           NLP.Concraft.Polish.DAG.Morphosyntax hiding (tag, Tag)
import qualified NLP.Concraft.Polish.DAG.Morphosyntax as PolX


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
    [tier1]
  where
    tier1 = D.Tier
      { D.withPos = True
      , D.withEos = True
      , D.withAtts = S.fromList ["cas", "per"]
      }


-------------------------------------------------
-- Tagging
-------------------------------------------------


data Tag = Tag
  { posiTag :: PolX.Tag
    -- ^ Positional tag (in textual form)
  , hasEos :: Bool
    -- ^ End-of-sentence marker
  } deriving (Show, Eq, Ord)


-- | Tag the sentence with guessing marginal probabilities.
guess :: C.Concraft Tag -> Sent Tag -> Sent Tag
guess = tagWith (C.guessMarginals . C.guesser)


-- | Tag the sentence with disambiguation marginal probabilities.
disamb :: C.Concraft Tag -> Sent Tag -> Sent Tag
disamb = tagWith (C.disambMarginals . C.disamb)


-- | Tag the sentence with disambiguation probabilities.
disamb' :: C.Concraft Tag -> D.ProbType -> Sent Tag -> Sent Tag
disamb' crf typ = tagWith (C.disambProbs typ . C.disamb) crf


-- | Perform guessing -> trimming -> disambiguation.
tag
  :: Int
  -- ^ Trimming parameter
  -> C.Concraft Tag
  -> Sent Tag
  -> Sent Tag
tag = tagWith . C.tag


-- -- | Perform guessing -> trimming -> disambiguation.
-- tag'
--   :: Int
--   -- ^ Trimming parameter
--   -> D.ProbType
--   -> C.Concraft
--   -> Sent Tag
--   -> Sent Tag
-- tag' k probTyp = tagWith (C.tag' k probTyp)


-- | Tag with the help of a lower-level annotation function.
tagWith
  -- :: (C.Concraft Tag -> X.Sent Word P.Tag -> C.Anno P.Tag Double)
  :: (C.Concraft Tag -> X.Sent Word Tag -> C.Anno Tag Double)
  -> C.Concraft Tag -> Sent Tag -> Sent Tag
tagWith annoFun concraft sent
  = fmap select
  $ DAG.zipE sent annoSent
  where
    select (edge, anno) = selectAnno anno edge
    annoSent = annoWith annoFun concraft sent


-- | Annotate with the help of a lower-level annotation function.
annoWith
  :: (C.Concraft Tag -> X.Sent Word Tag -> C.Anno Tag a)
  -> C.Concraft Tag -> Sent Tag -> C.Anno Tag a
annoWith anno concraft =
  anno concraft . packSent


-------------------------------------------------
-- High-level Tagging
-------------------------------------------------


-- | Annotated sentence.
data AnnoSent = AnnoSent
  { guessSent :: Sent Tag
  -- ^ The sentence after guessing and annotated with marginal probabilities
  , disambs   :: C.Anno Tag Bool
  -- ^ Disambiguation markers
  , marginals :: C.Anno Tag Double
  -- ^ Marginal probabilities according to the disambiguation model
  , maxProbs  :: C.Anno Tag Double
  -- ^ Maximal probabilities according to the disambiguation model
  }


-- | Annotate all possibly interesting information.
annoAll
  :: Int
  -- ^ Trimming parameter
  -> C.Concraft Tag
  -> Sent Tag
  -> AnnoSent
annoAll k concraft sent0 = AnnoSent
  { guessSent = _guessSent
  , disambs   = _disambs
  , marginals = _marginals
  , maxProbs  = _maxProbs }
  where
    _guessSent = tagWith (C.guess k . C.guesser) concraft sent0
    _marginals = annoWith (C.disambProbs D.Marginals . C.disamb) concraft _guessSent
    _maxProbs  = annoWith (C.disambProbs D.MaxProbs . C.disamb) concraft _guessSent
    _disambs   = C.disambPath (optimal _maxProbs) _maxProbs
    optimal = maybe [] id . listToMaybe . C.findOptimalPaths


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
    , r0        :: G.R0T
    -- | `G.zeroProbLabel` parameter
    , zeroProbLabel :: Tag
    }


-- | Train concraft model.
-- TODO: It should be possible to supply the two training procedures with
-- different SGD arguments.
train
    :: TrainConf
    -> IO [Sent Tag]      -- ^ Training data
    -> IO [Sent Tag]      -- ^ Evaluation data
    -> IO (C.Concraft Tag)
train TrainConf{..} train0 eval0 = do
--   let train1 = map (packSent tagset) <$> train0
--       eval1  = map (packSent tagset) <$> eval0
  let train1 = map packSent <$> train0
      eval1  = map packSent <$> eval0
  noReana train1 eval1
  where
    noReana tr ev = C.train tagset guessNum guessConf disambConf tr ev
    -- noReana tr ev = C.train tagset guessNum guessConf tr ev
    -- simplifyLabel = P.parseTag tagset
    simplifyGsr Tag{..} = P.parseTag tagset posiTag
    simplifyDmb Tag{..} = D.Tag
      { D.posiTag = P.parseTag tagset posiTag
      , D.hasEos = hasEos }
    guessConf = G.TrainConf guessSchemaDefault sgdArgs onDisk r0 zeroProbLabel simplifyGsr
    disambConf = D.TrainConf tiersDefault disambSchemaDefault sgdArgs onDisk simplifyDmb
