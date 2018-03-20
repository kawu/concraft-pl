{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}


import           Control.Applicative ((<$>))
import           Control.Monad (unless, forM_)
import           System.Console.CmdArgs
-- import           System.IO (hFlush, stdout)
import qualified Numeric.SGD.Momentum as SGD
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Tagset.Positional (parseTagset)
import qualified Data.Tagset.Positional as P
import qualified Data.Set as S

import qualified Data.DAG as DAG

import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Codec as CRF.Codec
import qualified Data.CRF.Chain1.Constrained.DAG.Train as CRF.Train

import qualified NLP.Concraft.DAG.Guess as Guess
import qualified NLP.Concraft.DAG.Schema as Schema
-- import qualified NLP.Concraft.DAG.Disamb as Disamb


-- import qualified NLP.Concraft.DAG2 as C
-- import qualified NLP.Concraft.Polish.DAG2 as P
import qualified NLP.Concraft.DAGSeg as C
import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Morphosyntax.Accuracy as Acc
import qualified NLP.Concraft.DAG.Segmentation as Seg
import qualified NLP.Concraft.Polish.DAG.Morphosyntax as PX
import qualified NLP.Concraft.Polish.DAGSeg as Pol
import qualified NLP.Concraft.Polish.DAG.Format.Base as DB

-- import qualified NLP.Concraft.Polish.Request as R

import           Paths_concraft_pl (version, getDataFileName)
import           Data.Version (showVersion)


---------------------------------------
-- Command line options
---------------------------------------


-- | A description of the Concraft-pl tool.
concraftDesc :: String
concraftDesc = "Concraft-dag-pl " ++ showVersion version


data Concraft
  = Train
    { trainPath     :: FilePath
    , evalPath      :: Maybe FilePath
    , tagsetPath    :: Maybe FilePath
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , disk          :: Bool
    , outModel      :: FilePath
    , guessNum      :: Int
    , r0            :: Guess.R0T
    , zeroProbLabel :: String
    , visibleOnly   :: Bool
    , disambTiers   :: Pol.DisambTiersCfg
    }
  | Tag
    { inModel       :: FilePath
    -- , marginals     :: Bool
    , probType      :: DB.ProbType
    -- , suppressProbs :: Bool
    , mayGuessNum   :: Maybe Int
    , shortestPath  :: Bool
    , longestPath   :: Bool
    , numericDisamb :: Bool }
  | Eval
    { justTagsetPath :: FilePath
    , goldPath       :: FilePath
    , taggPath       :: FilePath
    , onlyOov        :: Bool
    , onlyAmb        :: Bool
    , onlyEos        :: Bool
    , expandTags     :: Bool
    , ignoreTags     :: Bool
    , heedEos        :: Bool
    , weak           :: Bool
    , discardProb0   :: Bool
    }
  | Check
    { justTagsetPath :: FilePath
    , dagPath        :: FilePath
    }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    -- , discardHidden = False &= help "Discard hidden features"
    , iterNum = 20 &= help "Number of SGD iterations"
    , batchSize = 50 &= help
      "Batch size (the number of dataset elements taken in a single SGD update)"
    , regVar = 10.0 &=
      help "Regularization variance (the higher variance, the higher penalty for large params)"
    , gain0 = 0.25 &= help
      "Initial gain parameter (gain is used to scale the gradient before parameter update)"
      -- The value `0.25` makes sense given that SGD momentum is used
    , tau = 5.0 &= help
      "Initial tau parameter (after how many passes over the full dataset the gain is halved)"
    , disk = False &= help "Store SGD dataset on disk"
    , outModel = def &= typFile &= help "Output Model file"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word"
    , r0 = Guess.OovChosen &= help "R0 construction method"
    , zeroProbLabel = "xxx" &= help "Zero probability label"
    , visibleOnly = False &= help "Extract only visible features for the guesser"
    , disambTiers = Pol.TiersDefault &= help "Dismabiguation tiers configuration"
    }


tagMode :: Concraft
tagMode = Tag
    { inModel  = def &= argPos 0 &= typ "MODEL-FILE"
    -- , noAna    = False &= help "Do not analyse input text"
    -- , marginals = False &= help "Tag with marginal probabilities" }
    , probType = DB.Marginals &= help "Type of probabilities"
    -- , suppressProbs = False &= help "Do not show probabilities"
    , shortestPath = False &= help
      "Select shortest paths prior to parsing (can serve as a segmentation baseline)"
    , longestPath = False &= help
      "Select longest paths prior to parsing (mutually exclusive with shortestPath)"
    , mayGuessNum = def &= help "Number of guessed tags for each unknown word"
    , numericDisamb = False &= help
      "Print disamb markers as numerical values in the probability column"
    }


evalMode :: Concraft
evalMode = Eval
    { justTagsetPath = def &= typ "TAGSET-FILE"  &= argPos 0
    , goldPath = def &= typ "GOLD-FILE" &= argPos 1
    , taggPath = def &= typ "TAGGED-FILE" &= argPos 2
    , onlyOov  = False &= help "Only OOV edges"
    , onlyAmb  = False &= help "Only segmentation-ambiguous edges"
    , onlyEos = False &= help "Only EOS edges"
    , expandTags = False &= help "Expand tags"
    , ignoreTags = False &= help "Ignore tags (compute segmentation-level accurracy)"
    , heedEos = False &= help "Pay attention to EOS markers (ignored by default)"
    , weak = False &= help "Compute weak accuracy rather than strong"
    , discardProb0 = False &= help "Discard sentences with near 0 probability"
    }


checkMode :: Concraft
checkMode = Check
    { justTagsetPath = def &= typ "TAGSET-FILE"  &= argPos 0
    , dagPath= def &= typ "DAG-FILE" &= argPos 1
    }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [trainMode, tagMode, evalMode, checkMode]
    &= summary concraftDesc
    &= program "concraft-dag-pl"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgsRun argModes


exec :: Concraft -> IO ()


exec Train{..} = do
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/nkjp-tagset.cfg"
        Just x  -> return x
    tagset <- parseTagset tagsetPath' <$> readFile tagsetPath'
    -- let zeroProbLab = P.parseTag taset zeroProbLabel
    let zeroProbLab = PX.Interp
          { PX.base = "none"
          , PX.tag = T.pack zeroProbLabel
          , PX.commonness = Nothing
          , PX.qualifier = Nothing
          , PX.metaInfo = Nothing
          , PX.eos = False }
        train0 = DB.parseData <$> L.readFile trainPath
        eval0  = case evalPath of
          Nothing -> return []
          Just ph -> DB.parseData <$> L.readFile ph
    -- putStrLn $ "\nRegularization variance: " ++ show regVar
    concraft <- Pol.train (trainConf tagset zeroProbLab) train0 eval0
    unless (null outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        Pol.saveModel outModel concraft
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau
        }
    trainConf tagset zeroLab = Pol.TrainConf
        { tagset    = tagset
        , sgdArgs   = sgdArgs
        -- , reana     = not noAna
        , onDisk    = disk
        , guessNum  = guessNum
        , r0        = r0
        , zeroProbLabel = zeroLab
        , guessOnlyVisible = visibleOnly
        , disambTiersCfg = disambTiers
        }


exec Tag{..} = do
  -- crf <- Pol.loadModel P.parseTag inModel
  crf <- Pol.loadModel Pol.simplify4gsr Pol.simplify4dmb inModel
  inp <- DB.parseData <$> L.getContents
  let guessNum = case mayGuessNum of
        Nothing -> C.guessNum crf
        Just k  -> k
      cfg = Pol.AnnoConf
        { trimParam = guessNum
        , pickPath = case (shortestPath, longestPath) of
            (True, _) -> Just Seg.Min
            (_, True) -> Just Seg.Max
            _         -> Nothing
        }
      out = Pol.annoAll cfg crf <$> inp
      showCfg = DB.ShowCfg
        -- { suppressProbs = suppressProbs
        { probType = probType
        , numericDisamb = numericDisamb }
  L.putStr $ DB.showData showCfg out


exec Eval{..} = do
  tagset <- parseTagset justTagsetPath <$> readFile justTagsetPath
  let simplify = fmap $ \seg ->
        let simplify4eval interp =
              ( P.parseTag tagset $ PX.tag interp
              , PX.eos interp && heedEos )
            newTags = X.mapWMap simplify4eval (X.tags seg)
        in  seg {X.tags = newTags}
      process = PX.packSent . simplify
      fromFile = fmap (map process . DB.parseData) . L.readFile

  putStrLn $ concat
    [ "Note that in this evaulation only the tags (no lemmas,"
    , " no eos) are taken into account."
    ]

  let cfg = Acc.AccCfg
        { Acc.onlyOov = onlyOov
        , Acc.onlyAmb = onlyAmb
        , Acc.onlyMarkedWith =
          if onlyEos
          then S.singleton True
          else S.empty
        , Acc.accTagset = tagset
        , Acc.expandTag = expandTags
        , Acc.ignoreTag = ignoreTags
        , Acc.weakAcc = weak
        , Acc.discardProb0 = discardProb0 }
  stats <- Acc.collect cfg
    <$> fromFile goldPath
    <*> fromFile taggPath
  putStr "Precision: " >> print (Acc.precision stats)
  putStr "Recall: " >> print (Acc.recall stats)


exec Check{..} = do
  tagset <- parseTagset justTagsetPath <$> readFile justTagsetPath
  dags <- DB.parseData <$> L.readFile dagPath
  forM_ dags $ \dag -> do
    if (not $ DAG.isOK dag) then do
      putStrLn "Incorrectly structured graph:"
      showDAG dag
    else if (not $ DAG.isDAG dag) then do
      putStrLn "Graph with cycles:"
      showDAG dag
    else case verifyProb tagset dag of
      Nothing -> return ()
      Just p -> do
        putStr "Probability equal to "
        putStr (show p)
        putStrLn ":"
        showDAG dag
  where
    showDAG dag =
      forM_ (DAG.dagEdges dag) $ \edgeID -> do
        let from = DAG.begsWith edgeID dag
            to = DAG.endsWith edgeID dag
            val = DAG.edgeLabel edgeID dag
        putStr (show $ DAG.unNodeID from)
        putStr ", "
        putStr (show $ DAG.unNodeID to)
        putStr " => "
        T.putStrLn (PX.orth $ X.word val)
    verifyProb tagset dag =
      let schema = Schema.fromConf Schema.nullConf
          rawData = Guess.schemed (Pol.simplify4gsr tagset) schema [PX.packSent dag]
          [encDag] = CRF.Codec.encodeDataL (CRF.Codec.mkCodec rawData) rawData
          p = CRF.Train.dagProb encDag
          eps = 1e-9
      in  if p >= 1 - eps && p <= 1 + eps
          then Nothing
          else Just p


-- ---------------------------------------
-- -- Reading files
-- ---------------------------------------
--
-- -- TODO: make everything work on DAG input/output.
--
-- parseFileO' :: Format -> Maybe FilePath -> IO [X.SentO X.Tag]
-- parseFileO' format path = case path of
--     Nothing -> return []
--     Just pt -> parseFileO format pt
--
--
-- parseFileO :: Format -> FilePath -> IO [X.SentO X.Tag]
-- parseFileO format path = parseParaO format <$> L.readFile path
--
--
-- parseFile :: Format -> FilePath -> IO [X.Sent X.Tag]
-- parseFile format path = parsePara format <$> L.readFile path
--
--
-- ---------------------------------------
-- -- Parsing text
-- ---------------------------------------
--
--
-- -- parseTextO :: Format -> L.Text -> [[X.SentO X.Tag]]
-- -- parseTextO format = map (map X.withOrig) . parseText format
--
--
-- parseParaO :: Format -> L.Text -> [X.SentO X.Tag]
-- parseParaO format = map X.withOrig . parsePara format
--
--
-- ---------------------------------------
-- -- Parsing (format dependent)
-- ---------------------------------------
--
--
-- parseText :: Format -> L.Text -> [[X.Sent X.Tag]]
-- parseText Plain = P.parsePlain
--
--
-- parsePara :: Format -> L.Text -> [X.Sent X.Tag]
-- parsePara Plain = P.parsePara
--
--
-- ---------------------------------------
-- -- Showing (format dependent)
-- ---------------------------------------
--
--
-- data ShowCfg = ShowCfg {
--     -- | The format used.
--       formatCfg :: Format
--     -- | Show weights?
--     , showWsCfg :: Bool }
--
--
-- showData :: ShowCfg -> [[X.Sent X.Tag]] -> L.Text
-- showData ShowCfg{..} = P.showPlain (P.ShowCfg {P.showWsCfg = showWsCfg})
