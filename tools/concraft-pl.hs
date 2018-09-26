{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}


import           Control.Applicative ((<$>))
import           Control.Monad (unless, forM_)
import           System.FilePath (isAbsolute, (</>))
import           System.Console.CmdArgs
-- import           System.IO (hFlush, stdout)
import qualified Numeric.SGD.Momentum as SGD
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
-- import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BL
import           Data.Tagset.Positional (parseTagset)
import qualified Data.Tagset.Positional as P
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Dhall as Dhall

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
import qualified NLP.Concraft.Polish.DAG.Server as Server

-- import qualified NLP.Concraft.Polish.Request as R

import           Paths_concraft_pl (version, getDataFileName)
import           Data.Version (showVersion)


---------------------------------------
-- Command line options
---------------------------------------


-- | A description of the Concraft-pl tool.
concraftDesc :: String
concraftDesc = "Concraft-pl " ++ showVersion version


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
    -- , disambTiers   :: Pol.DisambTiersCfg
    , config        :: FilePath
    }
  | Tag
    { inModel       :: FilePath
    -- , marginals     :: Bool
    , inFile        :: Maybe FilePath
    , outFile       :: Maybe FilePath
    , blackFile     :: Maybe FilePath
    , probType      :: DB.ProbType
    -- , suppressProbs :: Bool
    , mayGuessNum   :: Maybe Int
    , freqPath      :: Maybe FilePath
    , freqSmoothing :: Double
    , shortestPath  :: Bool
    , longestPath   :: Bool
    , numericDisamb :: Bool
    }
  | Server
    { inModel       :: FilePath
    , blackFile     :: Maybe FilePath
    , probType      :: DB.ProbType
    , mayGuessNum   :: Maybe Int
    , numericDisamb :: Bool
    , port          :: Int
    }
  | Client
    { serverAddr    :: String
    , port           :: Int
    , inFile        :: Maybe FilePath
    , outFile       :: Maybe FilePath
    , batchSize     :: Int
    }
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
    , verbose        :: Bool
    }
  | Check
    { justTagsetPath :: FilePath
    , dagPath        :: FilePath
    }
  | Freqs
    { dagPath        :: FilePath
    -- , justTagsetPath :: FilePath
    -- , outPath        :: FilePath
    }
  | Ambi
    { dagPath        :: FilePath
    , onlyChosen     :: Bool
    }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , config = def &= typFile &= help "Global configuration file"
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
    -- , disambTiers = Pol.TiersDefault &= help "Dismabiguation tiers configuration"
    }


tagMode :: Concraft
tagMode = Tag
    { inModel  = def &= argPos 0 &= typ "MODEL-FILE"
    -- , noAna    = False &= help "Do not analyse input text"
    -- , marginals = False &= help "Tag with marginal probabilities" }
    , inFile  = def &= typFile &= help "Input file (stdin by default)"
    , outFile = def &= typFile &= help "Output file (stdout by default)"
    , blackFile = def &= typFile &= help "File with blacklisted tags, one per line"
    , probType = DB.Marginals &= help "Type of probabilities"
    -- , suppressProbs = False &= help "Do not show probabilities"
    , freqPath = def &= typFile &= help "File with chosen/not-chosen counts"
    , freqSmoothing = 1.0 &= help
      "Smoothing parameter for frequency-based path selection"
    , shortestPath = False &= help
      "Select shortest paths prior to parsing (can serve as a segmentation baseline)"
    , longestPath = False &= help
      "Select longest paths prior to parsing (mutually exclusive with shortestPath)"
    , mayGuessNum = def &= help "Number of guessed tags for each unknown word"
    , numericDisamb = False &= help
      "Print disamb markers as numerical values in the probability column"
    }


serverMode :: Concraft
serverMode = Server
    { inModel  = def &= argPos 0 &= typ "MODEL-FILE"
    , blackFile = def &= typFile &= help "File with blacklisted tags, one per line"
    , probType = DB.Marginals &= help "Type of probabilities"
    , mayGuessNum = def &= help "Number of guessed tags for each unknown word"
    , numericDisamb = False &= help
      "Print disamb markers as numerical values in the probability column"
    , port = 3000 &= help "Server port"
    }


clientMode :: Concraft
clientMode = Client
    { serverAddr = "http://localhost" &= help "Server address"
    , port = 3000 &= help "Server port number"
    , inFile  = def &= typFile &= help "Input file (stdin by default)"
    , outFile = def &= typFile &= help "Output file (stdout by default)"
    , batchSize = 10 &= help "Sent graphs in batches of the given size"
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
    , verbose = False &= help "Print information about compared elements"
    }


checkMode :: Concraft
checkMode = Check
    { justTagsetPath = def &= typ "TAGSET-FILE"  &= argPos 0
    , dagPath= def &= typ "DAG-FILE" &= argPos 1
    }


freqsMode :: Concraft
freqsMode = Freqs
    { dagPath = def &= typ "DAG-FILE" &= argPos 1
    -- , justTagsetPath = def &= typ "TAGSET-FILE"  &= argPos 0
    -- , outPath = def &= typ "FREQ-FILE" &= help "Output file to store counts"
    }


ambiMode :: Concraft
ambiMode = Ambi
    { dagPath = def &= typ "DAG-FILE" &= argPos 1
    , onlyChosen = False &= help "Take only the chose tokens into account"
    }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [ trainMode, tagMode, serverMode, clientMode
    , evalMode, checkMode, freqsMode, ambiMode ]
    &= summary concraftDesc
    &= program "concraft-pl"


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

    -- Dhall configuration
    let configPath =
          if isAbsolute config
          then config
          else "./" </> config
    dhall <- Dhall.detailed
      (Dhall.input Dhall.auto $ fromString configPath)

    -- let zeroProbLab = P.parseTag taset zeroProbLabel
    let zeroProbLab = PX.Interp
          { PX.base = "none"
          , PX.tag = T.pack zeroProbLabel
          , PX.commonness = Nothing
          , PX.qualifier = Nothing
          , PX.metaInfo = Nothing
          , PX.eos = False }
        train0 = DB.parseData <$> readFileUtf8 trainPath
        eval0  = case evalPath of
          Nothing -> return []
          Just ph -> DB.parseData <$> readFileUtf8 ph
    -- putStrLn $ "\nRegularization variance: " ++ show regVar
    concraft <- Pol.train (trainConf dhall tagset zeroProbLab) train0 eval0
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
    trainConf dhall tagset zeroLab = Pol.TrainConf
        { tagset    = tagset
        , sgdArgs   = sgdArgs
        -- , reana     = not noAna
        , onDisk    = disk
        , guessNum  = guessNum
        , r0        = r0
        , zeroProbLabel = zeroLab
        , guessOnlyVisible = visibleOnly
        -- , disambTiersCfg = disambTiers
        , globalConfig = dhall
        }


exec Tag{..} = do
  -- crf <- Pol.loadModel P.parseTag inModel
  crf <- Pol.loadModel Pol.simplify4gsr Pol.simplify4dmb inModel
  -- inp <- DB.parseData <$> L.getContents
  inp <- DB.parseData <$> case inFile of
    Nothing -> getContentsUtf8
    Just path -> readFileUtf8 path
  blackSet <-
    case blackFile of
      Nothing -> pure S.empty
      Just path -> readBlackSet path
  pathSelection <-
    case (shortestPath, longestPath, freqPath) of
      (True, _, _) -> return $ Just Seg.Min
      (_, True, _) -> return $ Just Seg.Max
      (_, _, Just freqPath') -> do
        freqMap <- loadFreqMap freqPath'
        let conf = Seg.FreqConf
              { Seg.pickFreqMap = freqMap
              , Seg.smoothingParam = freqSmoothing
              }
        return . Just $ Seg.Freq conf
      _         -> return Nothing
  let guessNum = case mayGuessNum of
        Nothing -> C.guessNum crf
        Just k  -> k
      cfg = Pol.AnnoConf
        { trimParam = guessNum
        , pickPath = pathSelection
        , blackSet = blackSet
        }
      out = Pol.annoAll cfg crf <$> inp
      showCfg = DB.ShowCfg
        -- { suppressProbs = suppressProbs
        { probType = probType
        , numericDisamb = numericDisamb }
  case outFile of
    Nothing -> putStrUtf8 $ DB.showData showCfg out
    Just path -> writeFileUtf8 path $ DB.showData showCfg out


exec Server{..} = do
  crf <- Pol.loadModel Pol.simplify4gsr Pol.simplify4dmb inModel
  blackSet <-
    case blackFile of
      Nothing -> pure S.empty
      Just path -> readBlackSet path
  let guessNum = case mayGuessNum of
        Nothing -> C.guessNum crf
        Just k  -> k
      cfg = Pol.AnnoConf
        { trimParam = guessNum
        , pickPath = Nothing
        , blackSet = blackSet
        }
      showCfg = DB.ShowCfg
        { probType = probType
        , numericDisamb = numericDisamb
        }
      serverCfg = Server.ServerCfg
        { concraft = crf
        , annoCfg = cfg
        , showCfg = showCfg
        }
  Server.runServer serverCfg port


exec Client{..} = do
  -- clear the file, if specified (stdout otherwise)
  case outFile of
    Nothing -> return ()
    Just path -> writeFileUtf8 path ""
  inpAll <- case inFile of
    Nothing -> getContentsUtf8
    Just path -> readFileUtf8 path
  let inputs
        = map (L.intercalate "\n\n")
        . group batchSize
        $ filter
          (not . L.null)
          (L.splitOn "\n\n" inpAll)
  forM_ (map L.toStrict inputs) $ \inp -> do
    let req = Server.Request {dag = inp}
        cfg = Server.ClientCfg {serverAddr=serverAddr, portNumber=port}
    Server.sendRequest cfg req >>= \case
      Nothing -> putStrLn "<< NO RESPONSE >>"
      Just Server.Answer{..} -> case outFile of
        Nothing -> putStrUtf8 (L.fromStrict dag)
        Just path -> appendFileUtf8 path (L.fromStrict dag)


exec Eval{..} = do
  tagset <- parseTagset justTagsetPath <$> readFile justTagsetPath
  let simplify = fmap $ \seg ->
        let simplify4eval interp =
              ( P.parseTag tagset $ PX.tag interp
              , PX.eos interp && heedEos )
            newTags = X.mapWMap simplify4eval (X.tags seg)
        in  seg {X.tags = newTags}
      process = PX.packSent . simplify
      fromFile = fmap (map process . DB.parseData) . readFileUtf8

  putStrLn $ concat
    [ "Note that in this evaluation lemmas "
    , "are *not* taken into account."
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
        , Acc.discardProb0 = discardProb0
        , Acc.verbose = verbose
        }
  stats <- Acc.collect cfg
    <$> fromFile goldPath
    <*> fromFile taggPath
  putStr "Precision: " >> print (Acc.precision stats)
  putStr "Recall: " >> print (Acc.recall stats)
  putStr "Accuracy: " >> print (Acc.accuracy stats)


exec Check{..} = do
  tagset <- parseTagset justTagsetPath <$> readFile justTagsetPath
  dags <- DB.parseData <$> readFileUtf8 dagPath
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


exec Freqs{..} = do
  -- tagset <- parseTagset justTagsetPath <$> readFile justTagsetPath
  dags <- DB.parseData <$> readFileUtf8 dagPath
  let freqMap = Seg.computeFreqs $ map PX.packSent dags
  printFreqMap freqMap


exec Ambi{..} = do
  dags <- DB.parseData <$> readFileUtf8 dagPath
  let stats = Seg.computeAmbiStats cfg $ map PX.packSent dags
  print stats
  where
    cfg = Seg.AmbiCfg
      { Seg.onlyChosen = onlyChosen
      }


---------------------------------------
-- Frequency map
---------------------------------------


printFreqMap
  :: M.Map T.Text (Int, Int)
  -> IO ()
printFreqMap freqMap =
  forM_ (M.toList freqMap) $ \(orth, (chosen, notChosen)) -> do
    T.putStr $ T.replace "\t" " " orth
    T.putStr "\t"
    putStr $ show chosen
    T.putStr "\t"
    putStrLn $ show notChosen


loadFreqMap
  :: FilePath
  -> IO (M.Map T.Text (Int, Int))
loadFreqMap filePath = do
  M.fromList . map readPair . T.lines <$> T.readFile filePath
  where
    readPair line =
      case T.splitOn "\t" line of
        [orth, chosen, notChosen] ->
          (orth, (readInt chosen, readInt notChosen))
        _ -> error $ "loadFreqMap: line incorrectly formatted: " ++ T.unpack line
    readInt = read . T.unpack


---------------------------------------
-- Blacklist
---------------------------------------


readBlackSet :: FilePath -> IO (S.Set T.Text)
readBlackSet path =
  S.fromList . map (L.toStrict . L.strip) . L.lines <$> readFileUtf8 path


---------------------------------------
-- UTF8
---------------------------------------


readFileUtf8 :: FilePath -> IO L.Text
readFileUtf8 path = L.decodeUtf8 <$> BL.readFile path


writeFileUtf8 :: FilePath -> L.Text -> IO ()
writeFileUtf8 path = BL.writeFile path . L.encodeUtf8


appendFileUtf8 :: FilePath -> L.Text -> IO ()
appendFileUtf8 path = BL.appendFile path . L.encodeUtf8


getContentsUtf8 :: IO L.Text
getContentsUtf8 = L.decodeUtf8 <$> BL.getContents


putStrUtf8 :: L.Text -> IO ()
putStrUtf8 = BL.putStr . L.encodeUtf8


---------------------------------------
-- Utils
---------------------------------------


-- | Group the input list into the groups of X.
group :: Int -> [a] -> [[a]]
group n =
  doit n []
  where
    doit k acc (x:xs)
      | k > 0 =
          doit (k-1) (x:acc) xs
      | otherwise =
          reverse acc : doit n [] (x:xs)
    doit _ acc []
      | null acc  = []
      | otherwise = [reverse acc]


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
