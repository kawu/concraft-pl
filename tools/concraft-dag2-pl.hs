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
import qualified NLP.Concraft.Polish.DAGSeg as Pol
import qualified NLP.Concraft.Polish.DAG.Morphosyntax as X
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
    , suppressProbs :: Bool
    , mayGuessNum   :: Maybe Int }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    -- , discardHidden = False &= help "Discard hidden features"
    , iterNum = 20 &= help "Number of SGD iterations"
    , batchSize = 50 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
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
    , suppressProbs = False &= help "Do not show probabilities"
    , mayGuessNum = def &= help "Number of guessed tags for each unknown word" }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [trainMode, tagMode]
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
    let zeroProbLab = X.Interp
          { X.base = "none"
          , X.tag = T.pack zeroProbLabel
          , X.commonness = Nothing
          , X.qualifier = Nothing
          , X.metaInfo = Nothing
          , X.eos = False }
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
        { suppressProbs = suppressProbs
        , probType = probType }
  L.putStr $ DB.showData showCfg out


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
