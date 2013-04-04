{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import qualified Numeric.SGD as SGD
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Tagset.Positional (parseTagset)

-- import NLP.Concraft.Format.Plain (plainFormat)
-- import qualified NLP.Concraft as C
-- import qualified NLP.Concraft.Schema as S
-- import qualified NLP.Concraft.Guess as G
-- import qualified NLP.Concraft.Disamb as D

import qualified NLP.Concraft.Polish as C
import qualified NLP.Concraft.Polish.Morphosyntax as X
import qualified NLP.Concraft.Polish.Format.Plain as P

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
    { format        :: Format
    , inModel       :: FilePath }
    -- , guessNum      :: Int }
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
    , format = enum [Plain &= help "Plain format"] }
    -- , guessNum = 10 &= help "Number of guessed tags for each unknown word" }

argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Concraft -> IO ()

exec Train{..} = do
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    train0 <- parseData  format trainPath
    eval0  <- parseData' format evalPath
    concraft <- C.train sgdArgs tagset guessNum train0 eval0 
    when (not . null $ outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        encodeFile outModel concraft
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec Tag{..} = do
    concraft <- decodeFile inModel
    out <- C.tag concraft <$> L.getContents
    L.putStr $ showData format out

parseData' :: Format -> Maybe FilePath -> IO (Maybe [X.SentO X.Tag])
parseData' format path = case path of
    Nothing -> return Nothing
    Just pt -> Just <$> parseData format pt

parseData :: Format -> FilePath -> IO [X.SentO X.Tag]
parseData Plain path = concat . P.parsePlainO C.ign <$> L.readFile path

showData :: Format -> [[X.Sent X.Tag]] -> L.Text
showData Plain = P.showPlain C.ign
