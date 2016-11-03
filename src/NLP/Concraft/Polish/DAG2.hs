{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | DAG-based model for morphosyntactic tagging.


module NLP.Concraft.Polish.DAG2
(
-- * Model
  C.Concraft
, C.saveModel
, C.loadModel

-- * Tagging
, guess
, disamb
, disamb'
, tag
, tag'
-- ** High level
, AnnoSent (..)
, annoAll

-- * Training
, TrainConf (..)
, train

-- * Pruning
, C.prune
) where


import           Control.Applicative ((<$>))
import           Control.Arrow (first)
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
import qualified NLP.Concraft.DAG.Disamb as D
import qualified NLP.Concraft.DAG2 as C

import           NLP.Concraft.Polish.DAG.Morphosyntax hiding (tag)


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


-- | Tag the sentence with guessing marginal probabilities.
guess :: C.Concraft -> Sent Tag -> Sent Tag
guess = tagWith (C.guessMarginals . C.guesser)


-- | Tag the sentence with disambiguation marginal probabilities.
disamb :: C.Concraft -> Sent Tag -> Sent Tag
disamb = tagWith (C.disambMarginals . C.disamb)


-- | Tag the sentence with disambiguation probabilities.
disamb' :: C.Concraft -> C.ProbType -> Sent Tag -> Sent Tag
disamb' crf typ = tagWith (C.disambProbs typ . C.disamb) crf


-- | Perform guessing -> trimming -> disambiguation.
tag
  :: Int
  -- ^ Trimming parameter
  -> C.Concraft
  -> Sent Tag
  -> Sent Tag
tag = tagWith . C.tag


-- | Perform guessing -> trimming -> disambiguation.
tag'
  :: Int
  -- ^ Trimming parameter
  -> D.ProbType
  -> C.Concraft
  -> Sent Tag
  -> Sent Tag
tag' k probTyp = tagWith (C.tag' k probTyp)


-- | Tag with the help of a lower-level annotation function.
tagWith
  :: (C.Concraft -> X.Sent Word P.Tag -> C.Anno P.Tag Double)
  -> C.Concraft -> Sent Tag -> Sent Tag
tagWith annoFun concraft sent
  = fmap select
  $ DAG.zipE sent annoSent
  where
    select (edge, anno) = selectAnno anno edge
    annoSent = annoWith (+) annoFun concraft sent

-- tagWith tagFun concraft sent
--     = fmap select
--     $ DAG.zipE sent annos
--   where
--     select (edge, anno) = selectAnno anno edge
--     tagset = C.tagset concraft
--     packed = packSent tagset sent
--     annos  = fmap
--         -- (X.mapWMap showTag)
--         (M.mapKeysWith (+) showTag)
--         (tagFun concraft packed)
--     showTag = P.showTag tagset


-- | Annotate with the help of a lower-level annotation function.
annoWith
  :: (a -> a -> a)
  -> (C.Concraft -> X.Sent Word P.Tag -> C.Anno P.Tag a)
  -> C.Concraft -> Sent Tag -> C.Anno Tag a
annoWith f anno concraft sent = fmap
  (M.mapKeysWith f showTag)
  (anno concraft packed)
  where
    showTag = P.showTag tagset
    packed = packSent tagset sent
    tagset = C.tagset concraft



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
  -> C.Concraft
  -> Sent Tag
  -> AnnoSent
annoAll k concraft sent0 = AnnoSent
  { guessSent = sent
  , disambs   = undefined
  , marginals = annoWith (+) (C.disambProbs D.Marginals . C.disamb) concraft sent
  , maxProbs  = annoWith (+) (C.disambProbs D.MaxProbs . C.disamb) concraft sent
  }
  where
    sent = tagWith (C.guess k . C.guesser) concraft sent0


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
    , zeroProbLabel :: Tag }

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
    noReana tr ev = C.train tagset guessNum guessConf disambConf tr ev
    guessConf  = G.TrainConf guessSchemaDefault sgdArgs onDisk r0 zeroProbTag
    disambConf = D.TrainConf tiersDefault disambSchemaDefault sgdArgs onDisk
    zeroProbTag = P.parseTag tagset zeroProbLabel
