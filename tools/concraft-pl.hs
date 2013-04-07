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

import qualified NLP.Concraft.Polish.Maca as Maca
import qualified NLP.Concraft.Polish as C
import qualified NLP.Concraft.Polish.Server as S
import qualified NLP.Concraft.Polish.Morphosyntax as X
import qualified NLP.Concraft.Polish.Format.Plain as P


-- | Default port number.
portDefault :: Int
portDefault = 10089


---------------------------------------
-- Command line options
---------------------------------------


-- | Data formats. 
data Format = Plain deriving (Data, Typeable, Show)


data Concraft
  = Train
    { trainPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , format        :: Format
    , tagsetPath    :: FilePath
    -- , discardHidden :: Bool
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , outModel      :: FilePath
    , guessNum      :: Int }
  | Tag
    { inModel       :: FilePath
    , format        :: Format }
    -- , guessNum      :: Int }
  | Server
    { inModel       :: FilePath
    , port          :: Int }
  | Client
    { format        :: Format
    , host          :: String
    , port          :: Int }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { tagsetPath = def &= argPos 0 &= typ "TAGSET-PATH"
    , trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , format = enum [Plain &= help "Plain format"]
    -- , discardHidden = False &= help "Discard hidden features"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outModel = def &= typFile &= help "Output Model file"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word" }


tagMode :: Concraft
tagMode = Tag
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , format  = enum [Plain &= help "Use plain format for output"] }
    -- , guessNum = 10 &= help "Number of guessed tags for each unknown word" }


serverMode :: Concraft
serverMode = Server
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , port    = portDefault &= help "Port number" }


clientMode :: Concraft
clientMode = Client
    { port   = portDefault &= help "Port number"
    , host   = "localhost" &= help "Server host name"
    , format = enum [Plain &= help "Use plain format for output"] }


argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes [trainMode, tagMode, serverMode, clientMode]


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgsRun argModes


exec :: Concraft -> IO ()


exec Train{..} = do
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    train0 <- parseData  format trainPath
    eval0  <- parseData' format evalPath
    concraft <- C.train sgdArgs tagset guessNum train0 eval0 
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


exec Tag{..} = do
    concraft <- C.loadModel inModel
    pool <- Maca.newMacaPool numCapabilities
    out <- C.tag' pool concraft =<< L.getContents
    L.putStr $ showData format out


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
    out <- S.tag' host portNum =<< L.getContents
    L.putStr $ showData format out


---------------------------------------
-- Parsing and showing
---------------------------------------


parseData' :: Format -> Maybe FilePath -> IO (Maybe [X.SentO X.Tag])
parseData' format path = case path of
    Nothing -> return Nothing
    Just pt -> Just <$> parseData format pt


parseData :: Format -> FilePath -> IO [X.SentO X.Tag]
parseData Plain path = concat . P.parsePlainO <$> L.readFile path


showData :: Format -> [[X.Sent X.Tag]] -> L.Text
showData Plain = P.showPlain
