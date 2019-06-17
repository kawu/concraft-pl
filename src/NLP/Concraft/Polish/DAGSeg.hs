{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- | DAG-based model for morphosyntactic tagging.


module NLP.Concraft.Polish.DAGSeg
(

-- * Types
  Tag
-- ** Simplification
, simplify4gsr
, complexify4gsr
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
, AnnoConf (..)
, annoAll

-- * Training
, TrainConf (..)
-- , DisambTiersCfg (..)
, train

-- * Pruning
-- , C.prune
) where


import           Prelude hiding (Word, pred)
import           Control.Applicative ((<$>))
-- import           Control.Arrow (first)
import           Control.Monad (guard)
-- import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
-- import           Data.Data (Data)
-- import           Data.Typeable (Typeable)

import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD.Momentum as SGD

import qualified Data.DAG as DAG

import qualified Data.CRF.Chain1.Constrained.DAG as CRF

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Morphosyntax.Ambiguous as XA
import qualified NLP.Concraft.DAG.Schema as S
import           NLP.Concraft.DAG.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.DAG.Guess as G
import qualified NLP.Concraft.DAG.DisambSeg as D
import qualified NLP.Concraft.DAG.Segmentation as Seg
import qualified NLP.Concraft.DAGSeg as C

import           NLP.Concraft.Polish.DAG.Morphosyntax hiding (tag, Tag)
import qualified NLP.Concraft.Polish.DAG.Morphosyntax as PolX
import qualified NLP.Concraft.Polish.DAG.Config as Cfg
import qualified NLP.Concraft.Polish.DAG.Config.Disamb as Cfg

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
    -- NOTE: The current configuration works quite well for segmentation.
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


-- | Default configuration for the disambiguation observation schema.
disambSchemaDefault :: SchemaConf
disambSchemaDefault = S.nullConf
    { lowOrthC      = entry                         [-2, -1, 0, 1]
    , lowPrefixesC  = oov $ entryWith [1, 2, 3]     [0]
    , lowSuffixesC  = oov $ entryWith [1, 2, 3]     [0]
    , begPackedC    = oov $ entry                   [0] }
  where
    oov (Just body) = Just $ body { S.oovOnly = True }
    oov Nothing     = Nothing


-- | Default tiered tagging configuration for the segmentation model.
tiersSegment :: [D.Tier]
tiersSegment =
    [tier]
  where
    tier = D.Tier
      { D.withPos = True
      , D.withEos = True
      , D.withAtts = S.fromList []
      }


-------------------------------------------------
-- Tagging Tiers
-------------------------------------------------


-- -- | Configuration of disambiguation tiers.
-- data DisambTiersCfg
--   = TiersDefault
--   | TiersGndCasSeparately
--   deriving (Data, Typeable, Show, Eq, Ord)
--
--
-- -- | Tiered tagging configuration for the disambiguation model.
-- tiersDisamb :: DisambTiersCfg -> [D.Tier]
-- tiersDisamb cfg = case cfg of
--   TiersDefault -> tiersDisambDefault
--   TiersGndCasSeparately -> tiersDisambGndCasSeparately
--
--
-- -- | Default tiered tagging configuration for the disambiguation model.
-- tiersDisambDefault :: [D.Tier]
-- tiersDisambDefault =
--     [tier1, tier2]
--   where
--     tier1 = D.Tier True False $ S.fromList
--       ["cas", "per"]
--     tier2 = D.Tier False False $ S.fromList
--       [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
--       , "acn", "ppr", "agg", "vlc", "dot"
--       , "sbg", "col"
--       ]
--
-- -- | Separate tier with gender and case values.
-- tiersDisambGndCasSeparately :: [D.Tier]
-- tiersDisambGndCasSeparately =
--     [tier1, tier2, tier3]
--   where
--     tier1 = D.Tier True False $ S.fromList
--       [ "per" ]
--     tier2 = D.Tier False False $ S.fromList
--       [ "nmb", "deg", "asp" , "ngt", "acm"
--       , "acn", "ppr", "agg", "vlc", "dot"
--       , "sbg", "col"
--       ]
--     tier3 = D.Tier False False $ S.fromList
--       [ "cas", "gnd"
--       ]


tiersDisamb :: Cfg.Config -> [D.Tier]
tiersDisamb Cfg.Config{..} = do
  Cfg.TierCfg{..} <- Cfg.tiersCfg disambCfg
  return $ D.Tier
    { D.withPos = withPos
    , D.withEos = withEos
    , D.withAtts = Cfg.unSet withAtts
    }


-------------------------------------------------
-- Tagging
-------------------------------------------------


-- type Tag = PolX.Tag
type Tag = PolX.Interp PolX.Tag


-- | Tag the sentence with guessing marginal probabilities.
guess :: CRF.Config P.Tag -> C.Concraft Tag -> Sent Tag -> Sent Tag
guess cfg = tagWith (C.guessMarginals cfg . C.guesser)


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


-- | Mark the word as EOS or not.
resolveEOS
  :: X.Seg Word Tag
  -> M.Map Tag Bool
  -> X.Seg Word Tag
resolveEOS seg selMap
  | isEos     = seg {X.tags = markEos}
  | otherwise = seg {X.tags = markNoEos}
  where
    isEos = not (null chosen) && all PolX.eos chosen
    chosen = [interp | (interp, True) <- M.toList selMap]
    -- Mark the segment as EOS or not
    markEos = X.mkWMap
      [ (interp {PolX.eos=True}, p)
      | (interp, p) <- M.toList (X.unWMap tagMap) ]
    markNoEos = X.mkWMap
      [ (interp {PolX.eos=False}, p)
      | (interp, p) <- M.toList (X.unWMap tagMap) ]
    tagMap = X.tags seg


-- | Decide if the word should be marked as eos or not, based on marginal
-- probabilities.
--
-- TODO: we don't need this heavy machinery any more.  This function is now
-- only used for training data preparation.
--
resolveEOS'
  :: Double
     -- ^ 0.5 means that the probability of the tag being EOS is twice as high
     -- as that of not being EOS. 1.0 means that EOS will be marked only if its
     -- 100% probable.
  -> X.Seg Word Tag
  -> X.Seg Word Tag
resolveEOS' minProp seg
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


-- -- | Configuration related to frequency-based path picking.
-- data PickFreqConf = PickFreqConf
--   { pickFreqMap :: Maybe (M.Map T.Text (Int, Int))
--     -- ^ A map which assigns (chosen, not chosen) counts to the invidiaul
--     -- orthographic forms.
--   , smoothingParam :: Double
--     -- ^ A naive smoothing related parameter, which should be adddd to each
--     -- count in `pickFreqMap`.
--   }


-- | Annotation config.
data AnnoConf = AnnoConf
  { trimParam :: Int
    -- ^ How many morphosyntactic tags should be kept for OOV words
  , pickPath :: Maybe Seg.PathTyp
    -- ^ Which path picking method should be used. The function takes the
  , blackSet :: S.Set T.Text
    -- ^ The set of blacklisted tags
  }


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
--   :: Int
--   -- ^ Trimming parameter
  :: AnnoConf
  -> C.Concraft Tag
  -> Sent Tag
  -> [AnnoSent]
-- annoAll k concraft sent0 =
annoAll AnnoConf{..} concraft sent00 =

  map annoOne _guessSent1

  where

    -- See whether the shortest path should be computed first
    sent0 =
      case pickPath of
        Just typ -> Seg.pickPath typ sent00
        Nothing -> sent00
  
    -- Parsed blacklisted tags and CRF config
    blackSet' =
      S.fromList . map (P.parseTag (C.tagset concraft)) . S.toList $ blackSet
    -- Make sure that the blackset is evaluated (otherwise, some malfored
    -- tags may be silentely ignored by the tool)
    crfCfg = length (show blackSet') `seq` CRF.Config {blackSet = blackSet'}

    -- We add EOS markers only after guessing, because the possible tags are not
    -- yet determined for the OOV words.
    ambiDag = XA.identifyAmbiguousSegments sent0
    _guessSent0 = DAG.mapE (addEosMarkers ambiDag) $
      tagWith (C.guess trimParam crfCfg . C.guesser) concraft sent0
    -- Resolve EOS tags based on the segmentation model
--     _guessSent1 = segment . fmap (resolveEOS' 0.5) $
--       tagWith (C.disambProbs D.MaxProbs . C.segmenter) concraft _guessSent0
    _guessSent1 = segment . fmap (uncurry resolveEOS) . DAG.zipE _guessSent0 $
      annoWith (C.disamb . C.segmenter) concraft _guessSent0

--     -- TEMP
--     _guessSent1 = (:[]) $
--       tagWith (C.guess trimParam crfCfg . C.guesser) concraft sent0
--       -- tagWith (const clearIt) concraft sent0
--     -- clearIt = fmap (fmap (const 0.0) . X.unWMap . X.tags) . DAG.mapN (const ())

    annoOne _guessSent = AnnoSent
      { guessSent = _guessSent
      , disambs   = _disambs
      , marginals = _marginals
      , maxProbs  = _maxProbs
--       , disambs   =  clearDAG _guessSent
--       , marginals = clearDAG _guessSent
--       , maxProbs  = clearDAG _guessSent
      }
      where
--         clearDAG = fmap (const M.empty) . DAG.mapN (const ())
        _marginals =
          annoWith (C.disambProbs D.Marginals . C.disamber) concraft _guessSent
        _maxProbs  =
          annoWith (C.disambProbs D.MaxProbs . C.disamber) concraft _guessSent
        _disambs   =
          annoWith (C.disamb . C.disamber) concraft _guessSent
--         _disambs   =
--           C.disambPath (optimal _maxProbs) _maxProbs
--           where optimal = maybe [] id . listToMaybe . C.findOptimalPaths


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
    -- | Extract only visible features for the guesser
    , guessOnlyVisible :: Bool
    -- | Global configuration
    , globalConfig :: Cfg.Config
    -- -- | Disambiguation tiers configuration
    -- , disambTiersCfg :: DisambTiersCfg
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

  putStr $ concat
    [ "Batch size for the "
    , "guessing and sentence segmentation models = "
    ]
  -- average number of sentences per paragraph
  averageParSize <- average . map (fromIntegral . length . segment) <$> trainR'IO
  let parBatchSize = ceiling $ fromIntegral (SGD.batchSize sgdArgs) / averageParSize
  print parBatchSize

  putStrLn "\n===== Train guessing model ====="
  guesser <- G.train (guessConf parBatchSize) trainR'IO evalR'IO
  let crfCfg = CRF.Config {blackSet = S.empty}
      doGuess = C.guessSent guessNum crfCfg guesser
      prepSent dag =
        let ambiDag = XA.identifyAmbiguousSegments dag
        in  DAG.mapE (addEosMarkers ambiDag) (doGuess dag)
      trainG'IO = map prepSent <$> trainR'IO
      evalG'IO  = map prepSent <$> evalR'IO

  putStrLn "\n===== Train sentence segmentation model ====="
  segmenter <- D.train (segmentConf parBatchSize) trainG'IO evalG'IO
  let prepSent' = segment . fmap (resolveEOS' 0.5)
      trainS'IO = concatMap prepSent' <$> trainG'IO
      evalS'IO  = concatMap prepSent' <$> evalG'IO

  putStrLn "\n===== Train disambiguation model ====="
  disamb <- D.train disambConf trainS'IO evalS'IO

  return $ C.Concraft tagset guessNum guesser segmenter disamb

  where

    guessConf batchSize = G.TrainConf
      guessSchemaDefault
      (sgdArgs {SGD.batchSize = batchSize})
      onDisk r0 zeroProbLabel
      (simplify4gsr tagset)
      (complexify4gsr tagset)
      strip4gsr
      guessOnlyVisible
    strip4gsr interp = PolX.voidInterp (PolX.tag interp)

    segmentConf batchSize = D.TrainConf
      tiersSegment segmentSchemaDefault
      (sgdArgs {SGD.batchSize = batchSize})
      onDisk
      (simplify4dmb tagset)

    disambConf = D.TrainConf
      -- (tiersDisamb disambTiersCfg)
      (tiersDisamb globalConfig)
      disambSchemaDefault sgdArgs onDisk
      (simplify4dmb tagset)


-- | Simplify the tag for the sake of the disambiguation model.
simplify4dmb :: P.Tagset -> PolX.Interp PolX.Tag -> D.Tag
simplify4dmb tagset PolX.Interp{..} = D.Tag
  { D.posiTag = P.parseTag tagset tag
  , D.hasEos = eos }


-- | Simplify the tag for the sake of the guessing model.
-- TODO: it is also used in the evaluation script, which assumes that
-- `simplify4gsr` simplifies to a positional tag. The name of the function
-- should reflect this, perhaps, or there should be two separate functions: one
-- dedicated to guesser, one dedicated to evaluation (and other more generic
-- things).
simplify4gsr :: P.Tagset -> PolX.Interp PolX.Tag -> P.Tag
simplify4gsr tagset PolX.Interp{..} = P.parseTag tagset tag


complexify4gsr :: P.Tagset -> P.Tag -> PolX.Interp PolX.Tag
complexify4gsr tagset tag = PolX.voidInterp (P.showTag tagset tag)


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


-- | Compute an average of the list.
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)
