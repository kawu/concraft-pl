{-# LANGUAGE RecordWildCards #-}

import           Control.Monad (unless)
import           Data.Binary (encodeFile, decodeFile)
import qualified Numeric.SGD as SGD
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Data.Tagset.Positional (parseTagset)

import           Options.Applicative

import qualified NLP.Concraft.Polish as C
import qualified NLP.Concraft.Polish.Morphosyntax as X
import qualified NLP.Concraft.Polish.Format.Plain as P

-- | Data formats. 
data Format = Plain
    deriving (Show, Read)

data TrainOpts = TrainOpts
    { tagsetPath    :: FilePath
    , trainPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , format        :: Format
    -- , discardHidden :: Bool
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , guessNum      :: Int
    , outModel      :: FilePath }
    deriving (Show)

-- | Change null path to `Nothing`. 
nullPath :: FilePath -> Maybe FilePath
nullPath "" = Nothing
nullPath xs = Just xs

trainOpts :: Parser TrainOpts
trainOpts = TrainOpts
    <$> argument str (metavar "TAGSET-FILE")
    <*> argument str (metavar "TRAIN-FILE")
    <*> (nullPath <$> strOption
        (  long "eval"
        <> short 'e'
        <> value ""
        <> help "Evaluation file" ))
    <*> option
        (  long "format"
        <> short 'f'
        <> value Plain
        <> help "Format" )
    <*> option
        (  long "iter"
        <> short 'i'
        <> value 10
        <> help "Number of SGD iterations" )
    <*> option
        (  long "batch"
        <> short 'b'
        <> value 30
        <> help "Batch size" )
    <*> option
        (  long "regvar"
        <> short 'r'
        <> value 10
        <> help "Regularization variance" )
    <*> option
        (  long "gain0"
        <> short '0'
        <> value 1
        <> help "Initial gain parameter" )
    <*> option
        (  long "tau"
        <> short 't'
        <> value 5
        <> help "Initial tau parameter" )
    <*> option
        (  long "guessNum"
        <> short 'g'
        <> value 10
        <> help "Initial tau parameter" )
    <*> strOption
        (  long "out"
        <> short 'o'
        <> metavar "FILE"
        <> value ""
        <> help "Output model file" )

data TagOpts = TagOpts
    { inModel       :: FilePath
    , formatT       :: Format }
    -- , guessNum      :: Int }
    deriving (Show)

tagOpts :: Parser TagOpts
tagOpts = TagOpts
    <$> argument str (metavar "MODEL-FILE")
    <*> option
        (  long "format"
        <> short 'f'
        <> value Plain
        <> help "Format" )

main :: IO ()
main = execParser opts >>= train
  where
    opts = info (helper <*> trainOpts)
        (  fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative" )

train :: TrainOpts -> IO ()
train TrainOpts{..} = do
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    train0 <- parseData  format trainPath
    eval0  <- parseData' format evalPath
    concraft <- C.train sgdArgs tagset guessNum train0 eval0 
    unless (null outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        encodeFile outModel concraft
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

-- data ConcraftOpts
--     = Train TrainOpts
--     | Tag TagOpts
--     deriving (Show)
-- 
-- concraftOpts :: Parser ConcraftOpts
-- concraftOpts = subparser
--     (  command "tag" (info (Tag <$> tagOpts) fullDesc)
--     <> command "train" (info (Train <$> trainOpts) fullDesc) )
-- 
-- main :: IO ()
-- main = execParser opts >>= exec
--   where
--     opts = info (helper <*> concraftOpts)
--         (  fullDesc
--         <> progDesc "Print a greeting for TARGET"
--         <> header "hello - a test for optparse-applicative" )
-- 
-- exec :: ConcraftOpts -> IO ()
-- 
-- exec (Train TrainOpts{..}) = do
--     tagset <- parseTagset tagsetPath <$> readFile tagsetPath
--     train0 <- parseData  format trainPath
--     eval0  <- parseData' format evalPath
--     concraft <- C.train sgdArgs tagset guessNum train0 eval0 
--     unless (null outModel) $ do
--         putStrLn $ "\nSaving model in " ++ outModel ++ "..."
--         encodeFile outModel concraft
--   where
--     sgdArgs = SGD.SgdArgs
--         { SGD.batchSize = batchSize
--         , SGD.regVar = regVar
--         , SGD.iterNum = iterNum
--         , SGD.gain0 = gain0
--         , SGD.tau = tau }
-- 
-- exec (Tag TagOpts{..}) = do
--     concraft <- decodeFile inModel
--     out <- C.tag concraft <$> L.getContents
--     L.putStr $ showData formatT out

parseData' :: Format -> Maybe FilePath -> IO (Maybe [X.SentO X.Tag])
parseData' format path = case path of
    Nothing -> return Nothing
    Just pt -> Just <$> parseData format pt

parseData :: Format -> FilePath -> IO [X.SentO X.Tag]
parseData Plain path = concat . P.parsePlainO P.ign <$> L.readFile path

showData :: Format -> [[X.Sent X.Tag]] -> L.Text
showData Plain = P.showPlain P.ign
