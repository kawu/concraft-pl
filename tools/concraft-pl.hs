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

import qualified NLP.Concraft.Polish.Maca as Maca
import qualified NLP.Concraft.Polish as C
import qualified NLP.Concraft.Polish.Server as S
import qualified NLP.Concraft.Polish.Morphosyntax as X
import qualified NLP.Concraft.Polish.Format.Plain as P

import           Paths_concraft_pl (version)
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
    , tagsetPath    :: FilePath
    , noAna         :: Bool
    -- , discardHidden :: Bool
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , onDisk        :: Bool
    , prune         :: Maybe Double
    , model         :: FilePath
    , guessNum      :: Int }
  | Tag
    { model         :: FilePath
    , noAna         :: Bool
    , format        :: Format }
    -- , guessNum      :: Int }
  | Server
    { model         :: FilePath
    , port          :: Int }
  | Client
    { format        :: Format
    , host          :: String
    , port          :: Int }
  | Compare
    { tagsetPath    :: FilePath
    , refPath       :: FilePath
    , otherPath     :: FilePath
    , format        :: Format }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { tagsetPath = def &= argPos 0 &= typ "TAGSET-PATH"
    , trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , format = enum [Plain &= help "Plain format"]
    , noAna = False &= help "Do not perform reanalysis"
    -- , discardHidden = False &= help "Discard hidden features"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , onDisk = False &= help "Store SGD dataset on disk"
    , prune = Nothing &= help "Disamb model pruning parameter"
    , model = def &= typFile &= help "Output Model file"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word" }


tagMode :: Concraft
tagMode = Tag
    { model     = def &= argPos 0 &= typ "MODEL-FILE"
    , noAna     = False &= help "Do not analyse input text"
    , format    = enum [Plain &= help "Plain input format"] }
    -- , guessNum = 10 &= help "Number of guessed tags for each unknown word" }


serverMode :: Concraft
serverMode = Server
    { model   = def &= argPos 0 &= typ "MODEL-FILE"
    , port    = portDefault &= help "Port number" }


clientMode :: Concraft
clientMode = Client
    { port   = portDefault &= help "Port number"
    , host   = "localhost" &= help "Server host name"
    , format = enum [Plain &= help "Plain output format"] }


compareMode :: Concraft
compareMode = Compare
    { tagsetPath = def &= argPos 0 &= typ "TAGSET-PATH"
    , refPath   = def &= argPos 1 &= typ "REFERENCE-FILE"
    , otherPath = def &= argPos 2 &= typ "OTHER-FILE"
    , format  = enum [Plain &= help "Plain input format"] }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [trainMode, tagMode, serverMode, clientMode, compareMode]
    &= summary concraftDesc
    &= program "concraft-pl"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgsRun argModes


exec :: Concraft -> IO ()


exec Train{..} = do
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    let train0 = parseFileO  format trainPath
    let eval0  = parseFileO' format evalPath
    concraft <- C.train (trainConf tagset) train0 eval0
    unless (null model) $ do
        putStrLn $ "\nSaving model in " ++ model ++ "..."
        C.saveModel model concraft
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
        , onDisk    = onDisk
        , guessNum  = guessNum
        , prune     = prune }


exec Tag{..} = do
    cnft <- C.loadModel model
    pool <- Maca.newMacaPool numCapabilities
    inp  <- L.getContents
    out  <- if not noAna
        then C.tag' pool cnft inp
        else return $
           let out = parseText format inp
           in  map (map (C.tagSent cnft)) out
    L.putStr $ showData format out


exec Server{..} = do
    putStr "Loading model..." >> hFlush stdout
    concraft <- C.loadModel model
    putStrLn " done"
    pool <- Maca.newMacaPool numCapabilities
    let portNum = N.PortNumber $ fromIntegral port
    putStrLn $ "Listening on port " ++ show port
    S.runConcraftServer pool concraft portNum


exec Client{..} = do
    let portNum = N.PortNumber $ fromIntegral port
    out <- S.tag' host portNum =<< L.getContents
    L.putStr $ showData format out


exec Compare{..} = do
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    let convert = map (X.packSegTag tagset) . concat
    xs <- convert <$> parseFile format refPath
    ys <- convert <$> parseFile format otherPath
    let s = Acc.weakLB tagset xs ys
    putStrLn $ "Number of segments in reference file: " ++ show (Acc.gold s)
    putStrLn $ "Number of correct tags: " ++ show (Acc.good s)
    putStrLn $ "Weak accuracy lower bound: " ++ show (Acc.accuracy s)


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
-- Parsing (format dependent functions)
---------------------------------------


parseText :: Format -> L.Text -> [[X.Sent X.Tag]]
parseText Plain = P.parsePlain


parsePara :: Format -> L.Text -> [X.Sent X.Tag]
parsePara Plain = P.parsePara


---------------------------------------
-- Showing data
---------------------------------------


showData :: Format -> [[X.Sent X.Tag]] -> L.Text
showData Plain = P.showPlain
