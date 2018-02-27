{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | DAG-based model for morphosyntactic tagging.


module NLP.Concraft.Polish.DAGSeg
(

-- * Types
  Tag (..)
-- ** Simplification
, simplify4gsr
, simplify4dmb

-- ** Model
, C.Concraft
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


import           Prelude hiding (Word)
import           Control.Applicative ((<$>))
import           Control.Arrow (first)
import           Control.Monad (guard)
import           Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD.Momentum as SGD

import qualified Data.DAG as DAG

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Morphosyntax.Ambiguous as XA
import qualified NLP.Concraft.DAG.Schema as S
import           NLP.Concraft.DAG.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.DAG.Guess as G
import qualified NLP.Concraft.DAG.DisambSeg as D
import qualified NLP.Concraft.DAGSeg as C

import           NLP.Concraft.Polish.DAG.Morphosyntax hiding (tag, Tag)
import qualified NLP.Concraft.Polish.DAG.Morphosyntax as PolX

-- import Debug.Trace (trace)


-------------------------------------------------
-- Default configuration
-------------------------------------------------


-- | Default configuration for the guessing observation schema.
guessSchemaDefault :: SchemaConf
guessSchemaDefault = S.nullConf
    { lowPrefixesC  = entryWith [1, 2]      [0]
    , lowSuffixesC  = entryWith [1, 2]      [0]
    , knownC        = entry                 [0]
    , begPackedC    = entry                 [0]
    }


-- | Default configuration for the segmentation observation schema.
segmentSchemaDefault :: SchemaConf
segmentSchemaDefault = S.nullConf
    { lowPrefixesC  = entryWith [1, 2]      [-1, 0, 1]
    , begPackedC    = entry                 [-1, 0, 1] }
    -- ^ NOTE: The current configuration works quite well for segmentation.
    -- Adding orthographic forms was not a good idea, at least not on a small
    -- training dataset.


-- -- | Default configuration for the guessing observation schema.
-- disambSchemaDefault :: SchemaConf
-- disambSchemaDefault = S.nullConf
--     { lowPrefixesC  = entryWith [1, 2]      [-1, 0, 1]
--     , begPackedC    = entry                 [-1, 0, 1] }


-- -- | Default configuration for the guessing observation schema.
-- disambSchemaDefault :: SchemaConf
-- disambSchemaDefault = S.nullConf
--     { orthC         = entry                   [-2, -1, 0, 1, 2]
--     -- { lowOrthC      = entry                   [-2, -1, 0, 1, 2]
--     , lowPrefixesC  = entryWith [1, 2, 3]     [-2, -1, 0, 1, 2]
--     , lowSuffixesC  = entryWith [1, 2, 3]     [-2, -1, 0, 1, 2]
--     , begPackedC    = entry                   [-2, -1, 0, 1, 2] }


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
tiersSegment :: [D.Tier]
tiersSegment =
    [tier]
  where
    tier = D.Tier
      { D.withPos = True
      , D.withEos = True
      , D.withAtts = S.fromList []
      }


-- | Default tiered tagging configuration.
tiersDisamb :: [D.Tier]
tiersDisamb =
    [tier1, tier2]
  where
    tier1 = D.Tier True False $ S.fromList ["cas", "per"]
    tier2 = D.Tier False False $ S.fromList
        [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
        , "acn", "ppr", "agg", "vlc", "dot" ]


-------------------------------------------------
-- Tagging
-------------------------------------------------


-- type Tag = PolX.Tag
type Tag = PolX.Interp PolX.Tag


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


-- | Tranform each tag into two tags, one with EOS(end-of-sentence)=True, one
-- with EOS=False. The idea behind this transformation is that, at some point,
-- we may want to tag with an EOS-aware model, with no EOS-related information
-- coming from the previous tagging stages.
--
-- NOTE: this procedure does not apply to OOV words. The motivation: it seems
-- highly unlikely that any OOV word can mark a sentence end.
--
-- NOTE: this procedure does not apply to segmentation-ambiguous words either.
-- Allowing the tagger to cut on such words might cause losing DAG edges.
addEosMarkers
  :: DAG.DAG a Bool
  -> DAG.EdgeID
  -> X.Seg Word Tag
  -> X.Seg Word Tag
addEosMarkers ambiDag edgeID seg
  | X.oov seg = seg
  | DAG.edgeLabel edgeID ambiDag = seg
  | otherwise = seg {X.tags = newTags (X.tags seg)}
  where
    newTags tagMap = X.mkWMap $ concat
      [ multiply interp p
      | (interp, p) <- M.toList (X.unWMap tagMap) ]
    multiply interp p
      | eos interp == True =
          [ (interp {eos=True}, p)
          , (interp {eos=False}, 0) ]
      | otherwise =
          [ (interp {eos=True}, 0)
          , (interp {eos=False}, p) ]


-- | Decide if the word should be marked as eos or not.
resolveEOS
  :: Double
     -- ^ 0.5 means that the probability of the tag being EOS is twice as high
     -- as that of not being EOS. 1.0 means that EOS will be marked only if its
     -- 100% probable.
  -> X.Seg Word Tag
  -> X.Seg Word Tag
resolveEOS minProp seg
  | isEos     = seg {X.tags = markEos}
  | otherwise = seg {X.tags = markNoEos}
  where
    tagMap = X.tags seg
    -- Determine the weights of the most probable EOS-marked and non-marked tags
    withEosW = maxWeightWith ((==True) . PolX.eos) tagMap
    withNoEosW = maxWeightWith ((==False) . PolX.eos) tagMap
    -- Should the segment be marked as EOS?
    isEos = case (withEosW, withNoEosW) of
      (Just eosW, Just noEosW) ->
        eosW / (eosW + noEosW) >= minProp
      (Just _, Nothing) -> True
      _ -> False
    -- Mark the segment as EOS or not
    markEos = X.mkWMap
      [ (interp {PolX.eos=True}, p)
      | (interp, p) <- M.toList (X.unWMap tagMap) ]
    markNoEos = X.mkWMap
      [ (interp {PolX.eos=False}, p)
      | (interp, p) <- M.toList (X.unWMap tagMap) ]


-- | Determine the weight of the most probable interpretation which satisfies
-- the given predicate.
maxWeightWith :: (Tag -> Bool) -> X.WMap Tag -> Maybe Double
maxWeightWith pred tagMap = mayMaximum
  [ p
  | (interp, p) <- M.toList (X.unWMap tagMap)
  , pred interp ]
  where
    mayMaximum [] = Nothing
    mayMaximum xs = Just $ maximum xs


-- | Try to segment the sentence based on the EOS markers.
-- segment :: X.Sent Word Tag -> [X.Sent Word Tag]
segment :: DAG.DAG a (X.Seg Word Tag) -> [DAG.DAG a (X.Seg Word Tag)]
segment sent =

  -- (\xs -> trace ("splits: " ++ show (length xs)) xs) $
  -- go (trace ("splitPoints: " ++ show splitPoints) splitPoints) sent
  go splitPoints sent

  where

    splitPoints = (S.toList . S.fromList)
      [ DAG.endsWith edgeID sent
      | edgeID <- DAG.dagEdges sent
      , interp <- M.keys . X.unWMap . X.tags . DAG.edgeLabel edgeID $ sent
      , PolX.eos interp ]

    go (splitPoint:rest) dag =
      case split splitPoint dag of
        Just (left, right) -> left : go rest right
        Nothing -> go rest dag
    go [] dag = [dag]

    split point dag = do
      (x, y) <- DAG.splitTmp point dag
      let empty = null . DAG.dagEdges
      guard . not . empty $ x
      guard . not . empty $ y
      return (x, y)


-------------------------------------------------
-- High-level Tagging
-------------------------------------------------


-- | Annotated sentence.
data AnnoSent = AnnoSent
  { guessSent :: Sent Tag
  -- ^ The sentence after guessing and segmentation
  -- (TODO: and annotated with marginal probabilities?)
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
  -> [AnnoSent]
annoAll k concraft sent0 =

  map annoOne _guessSent1

  where

    -- We add EOS markers only after guessing, because the possible tags are not
    -- yet determined for the OOV words.
    ambiDag = XA.identifyAmbiguousSegments sent0
    _guessSent0 = DAG.mapE (addEosMarkers ambiDag) $
      tagWith (C.guess k . C.guesser) concraft sent0
--     _guessSent0 = fmap addEosMarkers $
--       tagWith (C.guess k . C.guesser) concraft sent0
    -- Resolve EOS tags based on the segmentation model
    _guessSent1 = segment . fmap (resolveEOS 0.5) $
      tagWith (C.disambProbs D.MaxProbs . C.segmenter) concraft _guessSent0

    annoOne _guessSent = AnnoSent
      { guessSent = _guessSent
      , disambs   = _disambs
      , marginals = _marginals
      , maxProbs  = _maxProbs
      }
      where
        _marginals =
          annoWith (C.disambProbs D.Marginals . C.disamb) concraft _guessSent
        _maxProbs  =
          annoWith (C.disambProbs D.MaxProbs . C.disamb) concraft _guessSent
        _disambs   =
          C.disambPath (optimal _maxProbs) _maxProbs
          where optimal = maybe [] id . listToMaybe . C.findOptimalPaths


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
  let trainR'IO = map packSent <$> train0
      evalR'IO  = map packSent <$> eval0

  putStrLn "\n===== Train guessing model ====="
  guesser <- G.train guessConf trainR'IO evalR'IO
  let guess = C.guessSent guessNum guesser
      prepSent dag =
        let ambiDag = XA.identifyAmbiguousSegments dag
        in  DAG.mapE (addEosMarkers ambiDag) (guess dag)
      trainG'IO = map prepSent <$> trainR'IO
      evalG'IO  = map prepSent <$> evalR'IO

  putStrLn "\n===== Train segmentation model ====="
  segmenter <- D.train segmentConf trainG'IO evalG'IO
  let prepSent = segment . fmap (resolveEOS 0.5)
      trainS'IO = concatMap prepSent <$> trainG'IO
      evalS'IO  = concatMap prepSent <$> evalG'IO

  putStrLn "\n===== Train disambiguation model ====="
  disamb <- D.train disambConf trainS'IO evalS'IO

  return $ C.Concraft tagset guessNum guesser segmenter disamb

  where

    guessConf = G.TrainConf
      guessSchemaDefault sgdArgs onDisk r0 zeroProbLabel
      (simplify4gsr tagset) strip4gsr
    strip4gsr interp = PolX.voidInterp (PolX.tag interp)

    segmentConf = D.TrainConf
      tiersSegment segmentSchemaDefault sgdArgs onDisk
      (simplify4dmb tagset)

    disambConf = D.TrainConf
      tiersDisamb disambSchemaDefault sgdArgs onDisk
      (simplify4dmb tagset)


-- | Simplify the tag for the sake of the disambiguation model.
simplify4dmb :: P.Tagset -> PolX.Interp PolX.Tag -> D.Tag
simplify4dmb tagset PolX.Interp{..} = D.Tag
  { D.posiTag = P.parseTag tagset tag
  , D.hasEos = eos }


-- | Simplify the tag for the sake of the disambiguation model.
simplify4gsr :: P.Tagset -> PolX.Interp PolX.Tag -> P.Tag
simplify4gsr tagset PolX.Interp{..} = P.parseTag tagset tag


-- -- | Train the `Concraft` model.
-- -- No reanalysis of the input data will be performed.
-- --
-- -- The `FromJSON` and `ToJSON` instances are used to store processed
-- -- input data in temporary files on a disk.
-- train
--     :: (X.Word w, Ord t)
--     => P.Tagset             -- ^ A morphosyntactic tagset to which `P.Tag`s
--                             --   of the training and evaluation input data
--                             --   must correspond.
--     -> Int                  -- ^ How many tags is the guessing model supposed
--                             --   to produce for a given OOV word?  It will be
--                             --   used (see `G.guessSent`) on both training and
--                             --   evaluation input data prior to the training
--                             --   of the disambiguation model.
--     -> G.TrainConf t P.Tag  -- ^ Training configuration for the guessing model.
--     -> D.TrainConf t        -- ^ Training configuration for the
--                             --   disambiguation model.
--     -> IO [Sent w t]    -- ^ Training dataset.  This IO action will be
--                             --   executed a couple of times, so consider using
--                             --   lazy IO if your dataset is big.
--     -> IO [Sent w t]    -- ^ Evaluation dataset IO action.  Consider using
--                             --   lazy IO if your dataset is big.
--     -> IO (Concraft t)
-- train tagset guessNum guessConf disambConf trainR'IO evalR'IO = do
--   Temp.withTempDirectory "." ".guessed" $ \tmpDir -> do
--   let temp = withTemp tagset tmpDir
--
--   putStrLn "\n===== Train guessing model ====="
--   guesser <- G.train guessConf trainR'IO evalR'IO
--   let guess = guessSent guessNum guesser
--   trainG  <- map guess <$> trainR'IO
--   evalG   <- map guess <$> evalR'IO
--
--   temp "train" trainG $ \trainG'IO -> do
--   temp "eval"  evalG  $ \evalG'IO  -> do
--
--   putStrLn "\n===== Train disambiguation model ====="
--   disamb <- D.train disambConf trainG'IO evalG'IO
--   return $ Concraft tagset guessNum guesser disamb
