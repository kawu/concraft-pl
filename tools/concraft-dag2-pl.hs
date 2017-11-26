{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (unless)
import           System.Console.CmdArgs
-- import           System.IO (hFlush, stdout)
import qualified Numeric.SGD.Momentum as SGD
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Tagset.Positional (parseTagset)
-- import qualified Data.Tagset.Positional as P

import qualified NLP.Concraft.DAG.Guess as Guess
-- import qualified NLP.Concraft.DAG.Disamb as Disamb

-- import qualified NLP.Concraft.DAG2 as C
-- import qualified NLP.Concraft.Polish.DAG2 as P
import qualified NLP.Concraft.DAGSeg as C
import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.DAG.Morphosyntax.Accuracy as Acc
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
    , zeroProbLabel :: String }
  | Tag
    { inModel       :: FilePath
    -- , marginals     :: Bool
    , probType      :: DB.ProbType
    -- , suppressProbs :: Bool
    , mayGuessNum   :: Maybe Int
    , numericDisamb :: Bool }
  | Eval
    { justTagsetPath :: FilePath
    , goldPath       :: FilePath
    , taggPath       :: FilePath
    , onlyOov        :: Bool
    , expandTags     :: Bool
    , weak           :: Bool
    , discardProb0   :: Bool
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
    , zeroProbLabel = "xxx" &= help "Zero probability label" }


tagMode :: Concraft
tagMode = Tag
    { inModel  = def &= argPos 0 &= typ "MODEL-FILE"
    -- , noAna    = False &= help "Do not analyse input text"
    -- , marginals = False &= help "Tag with marginal probabilities" }
    , probType = DB.Marginals &= help "Type of probabilities"
    -- , suppressProbs = False &= help "Do not show probabilities"
    , mayGuessNum = def &= help "Number of guessed tags for each unknown word"
    , numericDisamb = False &= help
      "Print disamb markers as numerical values in the probability column"
    }


evalMode :: Concraft
evalMode = Eval
    { justTagsetPath = def &= typ "TAGSET-FILE"  &= argPos 0
    , goldPath = def &= typ "GOLD-FILE" &= argPos 1
    , taggPath = def &= typ "TAGGED-FILE" &= argPos 2
    , onlyOov   = False &= help "Only OOV segments"
    , expandTags = False &= help "Expand tags"
    , weak = False &= help "Compute weak accuracy rather than strong"
    , discardProb0 = False &= help "Discard sentences with near 0 probability"
    }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [trainMode, tagMode, evalMode]
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
        , SGD.tau = tau }
    trainConf tagset zeroLab = Pol.TrainConf
        { tagset    = tagset
        , sgdArgs   = sgdArgs
        -- , reana     = not noAna
        , onDisk    = disk
        , guessNum  = guessNum
        , r0        = r0
        , zeroProbLabel = zeroLab }


exec Tag{..} = do
  -- crf <- Pol.loadModel P.parseTag inModel
  crf <- Pol.loadModel Pol.simplify4gsr Pol.simplify4dmb inModel
  inp <- DB.parseData <$> L.getContents
  let guessNum = case mayGuessNum of
        Nothing -> C.guessNum crf
        Just k  -> k
      -- out = Pol.tag' guessNum (mkProbType probType) crf <$> inp
      out = Pol.annoAll guessNum crf <$> inp
      showCfg = DB.ShowCfg
        -- { suppressProbs = suppressProbs
        { probType = probType
        , numericDisamb = numericDisamb }
  L.putStr $ DB.showData showCfg out


exec Eval{..} = do
  tagset <- parseTagset justTagsetPath <$> readFile justTagsetPath
  let simplify = fmap $ \seg ->
        let newTags = X.mapWMap (Pol.simplify4gsr tagset) (X.tags seg)
        in  seg {X.tags = newTags}
      process = PX.packSent . simplify
      fromFile = fmap (map process . DB.parseData) . L.readFile

  putStrLn $ concat
    [ "Note that in this evaulation only the tags (no lemmas,"
    , " no eos) are taken into account."
    ]

  let cfg = Acc.AccCfg
        { Acc.accSel = if onlyOov then Acc.Oov else Acc.All
        , Acc.accTagset = tagset
        , Acc.expandTag = expandTags
        , Acc.weakAcc = weak
        , Acc.discardProb0 = discardProb0 }
  stats <- Acc.collect cfg
    <$> fromFile goldPath
    <*> fromFile taggPath
  putStr "Precision: " >> print (Acc.precision stats)
  putStr "Recall: " >> print (Acc.recall stats)


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
