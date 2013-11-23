{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (unless)
import           System.Console.CmdArgs
import           System.IO (hFlush, stdout)
import qualified Network as N
import qualified Numeric.SGD as SGD
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Tagset.Positional (parseTagset)
import           GHC.Conc (numCapabilities)

import qualified NLP.Concraft.Morphosyntax.Accuracy as Acc
import qualified NLP.Concraft.Guess as Guess

import qualified NLP.Concraft.Polish.Maca as Maca
import qualified NLP.Concraft.Polish as C
import qualified NLP.Concraft.Polish.Request as R
import qualified NLP.Concraft.Polish.Server as S
import qualified NLP.Concraft.Polish.Morphosyntax as X
import qualified NLP.Concraft.Polish.Format.Plain as P

import           Paths_concraft_pl (version, getDataFileName)
import           Data.Version (showVersion)


-- | Default port number.
portDefault :: Int
portDefault = 10089


---------------------------------------
-- Command line options
---------------------------------------


-- | Data formats.
data Format = Plain deriving (Data, Typeable, Show)


-- | A description of the Concraft-pl tool
concraftDesc :: String
concraftDesc = "Concraft-pl " ++ showVersion version


data Concraft
  = Train
    { trainPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , format        :: Format
    , tagsetPath    :: Maybe FilePath
    , noAna         :: Bool
    -- , discardHidden :: Bool
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , disk          :: Bool
    , outModel      :: FilePath
    , guessNum      :: Int
    , r0            :: Guess.R0T }
  | Tag
    { inModel       :: FilePath
    , noAna         :: Bool
    , format        :: Format
    , marginals     :: Bool }
    -- , guessNum      :: Int }
  | Server
    { inModel       :: FilePath
    , port          :: Int }
  | Client
    { noAna         :: Bool
    , format        :: Format
    , marginals     :: Bool
    , host          :: String
    , port          :: Int }
  | Compare
    { tagsetPath    :: Maybe FilePath
    , refPath       :: FilePath
    , otherPath     :: FilePath
    , format        :: Format }
  | Prune
    { inModel       :: FilePath
    , outModel      :: FilePath
    , threshold     :: Double }
--   | ReAna
--     { format	    :: Format }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , format = enum [Plain &= help "Plain format"]
    , noAna = False &= help "Do not perform reanalysis"
    -- , discardHidden = False &= help "Discard hidden features"
    , iterNum = 20 &= help "Number of SGD iterations"
    , batchSize = 50 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , disk = False &= help "Store SGD dataset on disk"
    , outModel = def &= typFile &= help "Output Model file"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word"
    , r0 = Guess.OovChosen &= help "R0 construction method" }


tagMode :: Concraft
tagMode = Tag
    { inModel  = def &= argPos 0 &= typ "MODEL-FILE"
    , noAna    = False &= help "Do not analyse input text"
    , format   = enum [Plain &= help "Plain format"]
    , marginals = False &= help "Tag with marginal probabilities" }
    -- , guessNum = 10 &= help "Number of guessed tags for each unknown word" }


serverMode :: Concraft
serverMode = Server
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , port    = portDefault &= help "Port number" }


clientMode :: Concraft
clientMode = Client
    { noAna   = False &= help "Do not perform reanalysis"
    , port    = portDefault &= help "Port number"
    , host    = "localhost" &= help "Server host name"
    , format  = enum [Plain &= help "Plain output format"]
    , marginals = False &= help "Tag with marginal probabilities" }


compareMode :: Concraft
compareMode = Compare
    { refPath   = def &= argPos 1 &= typ "REFERENCE-FILE"
    , otherPath = def &= argPos 2 &= typ "OTHER-FILE"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , format  = enum [Plain &= help "Plain format"] }


pruneMode :: Concraft
pruneMode = Prune
    { inModel   = def &= argPos 0 &= typ "INPUT-MODEL"
    , outModel  = def &= argPos 1 &= typ "OUTPUT-MODEL"
    , threshold = 0.05 &=
        help "Remove disambiguation features below the threshold" }


-- reAnaMode :: Concraft
-- reAnaMode = ReAna
--     { format    = enum [Plain &= help "Plain format"] }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [trainMode, tagMode, serverMode, clientMode, compareMode, pruneMode]
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
    let train0 = parseFileO  format trainPath
    let eval0  = parseFileO' format evalPath
    concraft <- C.train (trainConf tagset) train0 eval0
    unless (null outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        C.saveModel outModel concraft
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }
    trainConf tagset = C.TrainConf
        { tagset    = tagset 
        , sgdArgs   = sgdArgs
        , reana     = not noAna
        , onDisk    = disk
        , guessNum  = guessNum
        , r0        = r0 }


exec Tag{..} = do
    cft <- C.loadModel inModel
    pool <- Maca.newMacaPool numCapabilities
    inp <- L.getContents
    out <- R.long (R.short pool cft) $ rq $ if noAna
        then R.Doc $ parseText format inp
        else R.Long inp
    L.putStr $ showData showCfg out
  where
    rq x = R.Request
        { R.rqBody = x
        , R.rqConf = rqConf }
    rqConf = R.Config
        { R.tagProbs = marginals }
    showCfg = ShowCfg
        { formatCfg = format
        , showWsCfg = marginals }


exec Server{..} = do
    putStr "Loading model..." >> hFlush stdout
    concraft <- C.loadModel inModel
    putStrLn " done"
    pool <- Maca.newMacaPool numCapabilities
    let portNum = N.PortNumber $ fromIntegral port
    putStrLn $ "Listening on port " ++ show port
    S.runConcraftServer pool concraft portNum


exec Client{..} = do
    let portNum = N.PortNumber $ fromIntegral port
    inp <- L.getContents
    out <- R.long (S.submit host portNum) $ rq $ if noAna
        then R.Doc $ parseText format inp
        else R.Long inp
    L.putStr $ showData showCfg out
  where
    rq x = R.Request
        { R.rqBody = x
        , R.rqConf = rqConf }
    rqConf = R.Config
        { R.tagProbs = marginals }
    showCfg = ShowCfg
        { formatCfg = format
        , showWsCfg = marginals }


exec Compare{..} = do
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/nkjp-tagset.cfg" 
        Just x  -> return x
    tagset <- parseTagset tagsetPath' <$> readFile tagsetPath'
    let convert = map (X.packSeg tagset) . concat
    xs <- convert <$> parseFile format refPath
    ys <- convert <$> parseFile format otherPath
    let s = Acc.weakLB tagset xs ys
    putStrLn $ "Number of segments in reference file: " ++ show (Acc.gold s)
    putStrLn $ "Number of correct tags: " ++ show (Acc.good s)
    putStrLn $ "Weak accuracy lower bound: " ++ show (Acc.accuracy s)


exec Prune{..} = do
    cft <- C.loadModel inModel
    C.saveModel outModel $ C.prune threshold cft


-- exec ReAna{..} = do
--     inp  <- parseText format <$> L.getContents
--     out  <- showData format <$> 


---------------------------------------
-- Reading files
---------------------------------------


parseFileO' :: Format -> Maybe FilePath -> IO [X.SentO X.Tag]
parseFileO' format path = case path of
    Nothing -> return []
    Just pt -> parseFileO format pt


parseFileO :: Format -> FilePath -> IO [X.SentO X.Tag]
parseFileO format path = parseParaO format <$> L.readFile path


parseFile :: Format -> FilePath -> IO [X.Sent X.Tag]
parseFile format path = parsePara format <$> L.readFile path


---------------------------------------
-- Parsing text
---------------------------------------


-- parseTextO :: Format -> L.Text -> [[X.SentO X.Tag]]
-- parseTextO format = map (map X.withOrig) . parseText format


parseParaO :: Format -> L.Text -> [X.SentO X.Tag]
parseParaO format = map X.withOrig . parsePara format


---------------------------------------
-- Parsing (format dependent)
---------------------------------------


parseText :: Format -> L.Text -> [[X.Sent X.Tag]]
parseText Plain = P.parsePlain


parsePara :: Format -> L.Text -> [X.Sent X.Tag]
parsePara Plain = P.parsePara


---------------------------------------
-- Showing (format dependent)
---------------------------------------


data ShowCfg = ShowCfg {
    -- | The format used.
      formatCfg :: Format
    -- | Show weights?
    , showWsCfg :: Bool }


showData :: ShowCfg -> [[X.Sent X.Tag]] -> L.Text
showData ShowCfg{..} = P.showPlain (P.ShowCfg {P.showWsCfg = showWsCfg})
