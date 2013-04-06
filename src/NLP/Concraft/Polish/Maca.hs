{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | The module provides interface for the Maca analysis tool.
-- See <http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki>
-- for more information about the analyser.


module NLP.Concraft.Polish.Maca
(
-- * Types
  Maca

-- * Server
, newMacaServer

-- * Client
, macaPar
) where


import           Control.Applicative ((<$>))
import           Control.Monad (void, forever)
import           Control.Concurrent
import           Control.Exception
import           System.Process
import           System.IO
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import           NLP.Concraft.Polish.Morphosyntax hiding (restore)
import qualified NLP.Concraft.Polish.Format.Plain as Plain


----------------------------
-- Types
----------------------------


-- | Input channel.
type In = Chan T.Text


-- | Output channel.
type Out = Chan [Sent Tag]


-- | Maca communication channels.
newtype Maca = Maca (In, Out)


----------------------------
-- Server
----------------------------


-- | Create Maca server with two communication channels.
newMacaServer :: IO Maca
newMacaServer = do
    inCh  <- newChan
    outCh <- newChan
    void $ runMaca inCh outCh
    return $ Maca (inCh, outCh)


-- | Run Maca server on given channels.
-- TODO: Should check, if maca works.  In particular, if morfeusz is available.
runMaca :: In -> Out -> IO ThreadId
runMaca inCh outCh = forkIO . mask $ \restore -> do
    let cmd  = "maca-analyse"
        args = ["-q", "morfeusz-nkjp-official", "-o", "plain", "-l"]
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe
                                     , std_out = CreatePipe
                                     , std_err = CreatePipe }

    let excHandler = do
            err <- hGetContents errh
            putStr "Error: " >> putStrLn err
            hClose inh; hClose outh; hClose errh
            terminateProcess pid
            waitForProcess pid

    hSetBuffering outh LineBuffering
    flip onException excHandler $ restore $ forever $ do

        -- Take element from the input channel.
        txt <- readChan inCh

        -- Write text to maca stdin.
        -- TODO: Handle the "empty" case.
        T.hPutStr inh txt; hFlush inh

        -- Number of non-space characters in the input.
        let n = T.length $ T.filter (not . C.isSpace) txt

        -- Read maca response and put it in the output channel.
        writeChan outCh =<< readMacaResponse outh n


readMacaResponse :: Handle -> Int -> IO [Sent Tag]
readMacaResponse h n
    | n <= 0    = return []
    | otherwise = do
        x  <- readMacaSent h
        xs <- readMacaResponse h (n - charNum x)
        return (x : xs)


readMacaSent :: Handle -> IO (Sent Tag)
readMacaSent h =
    Plain.parseSent Plain.ign <$> getTxt 
  where
    getTxt = do
        x <- L.hGetLine h
        if L.null x
            then return x
            else do   
                xs <- getTxt
                return (x `L.append` "\n" `L.append` xs)


-- | A number of non-space characters in a sentence.
charNum :: Sent t -> Int
charNum =
    let wordCharNum Word{..} = T.length $ T.filter (not . C.isSpace) orth
    in  sum . map (wordCharNum . word)


----------------------------
-- Client
----------------------------


-- TODO:
-- * Make certain, that input text is terminated with '\n'. 
-- * All '\n' characters should be changed to e.g. "  ".
-- * Info about newlines ('\n' characters) should be restored.


-- | Analyse paragraph with Maca.
macaPar :: Maca -> T.Text -> IO [Sent Tag]
macaPar (Maca (inCh, outCh)) x = do
    writeChan inCh x
    readChan outCh
