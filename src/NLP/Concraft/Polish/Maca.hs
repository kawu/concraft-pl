{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | The module provides interface for the Maca analysis tool.
-- See <http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki>
-- for more information about the analyser.


module NLP.Concraft.Polish.Maca
(
  MacaPool
, newMacaPool
, macaPar
) where


import           Control.Applicative ((<$>))
import           Control.Monad (void, forever, guard, replicateM, unless)
import           Control.Concurrent
import           Control.Exception
import           System.Process
import           System.IO
import           Data.Function (on)
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Trans.Maybe as M
import           Control.Monad.Trans.Class (lift)

import           NLP.Concraft.Polish.Morphosyntax hiding (restore)
import qualified NLP.Concraft.Polish.Format.Plain as Plain


----------------------------
-- Maca instance
----------------------------


-- TODO: We don't have to use channels here.  Maximum one element
-- should be present in the input/output channel.
    

-- | Input channel.
type In = Chan T.Text


-- | Output channel.
type Out = Chan [Sent Tag]


-- | Maca communication channels.
newtype Maca = Maca (In, Out)


-- | Run Maca instance.
newMaca :: IO Maca
newMaca = do
    inCh  <- newChan
    outCh <- newChan
    void $ runMacaOn inCh outCh
    return $ Maca (inCh, outCh)


-- | Run Maca server on given channels.
-- TODO: Should check, if maca works.  In particular, if morfeusz is available.
runMacaOn :: In -> Out -> IO ThreadId
runMacaOn inCh outCh = mask $ \restore -> forkIO (do
    let cmd  = "maca-analyse"
        args = ["-q", "morfeusz-nkjp-official", "-o", "plain", "-l"]
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe
                                     , std_out = CreatePipe
                                     , std_err = CreatePipe }

    let excHandler = do
            let tryIO = try :: IO a -> IO (Either IOException a)
            void $ tryIO $ do
                err <- hGetContents errh
                unless (all C.isSpace err) $ do
                    putStr "Maca error: "
                    putStrLn err
            hClose inh; hClose outh; hClose errh
            terminateProcess pid
            waitForProcess pid

    -- TODO: Document, why LineBuffering is needed here.
    hSetBuffering outh LineBuffering
    flip onException excHandler $ restore $ forever $ do

        -- Take element from the input channel.
        txt <- readChan inCh
        -- putStr "REQUEST: "
        -- print txt

        -- Write text to maca stdin.
        -- TODO: Handle the "empty" case?
        T.hPutStr inh txt; hFlush inh

        -- Read maca response and put it in the output channel.
        writeChan outCh =<< readMacaResponse outh (textWeight txt)
    )


readMacaResponse :: Handle -> Int -> IO [Sent Tag]
readMacaResponse h n
    | n <= 0    = return []
    | otherwise = do
        x  <- readMacaSent h
        xs <- readMacaResponse h (n - sentWeight x)
        return (x : xs)


readMacaSent :: Handle -> IO (Sent Tag)
readMacaSent h =
    Plain.parseSent . L.unlines <$> getTxt
  where
    getTxt = do
        x <- L.hGetLine h
        if L.null x
            then return []
            else (x:) <$> getTxt


----------------------------
-- Client
----------------------------
    

-- | Analyse paragraph with Maca.
doMacaPar :: Maca -> T.Text -> IO [Sent Tag]
doMacaPar (Maca (inCh, outCh)) par = do
    let par' = T.intercalate "  " (T.lines par) `T.append` "\n"
    writeChan inCh par'
    restoreSpaces par <$> readChan outCh


-- | Restore info abouts spaces from a text and insert them
-- to a parsed paragraph.
restoreSpaces :: T.Text -> [Sent Tag] -> [Sent Tag]
restoreSpaces par sents =
    S.evalState (mapM onSent sents) (0, chunks)
  where
    -- For each space chunk in the paragraph compute
    -- total weight of earlier chunks.
    parts   = T.groupBy ((==) `on` C.isSpace) par
    weights = scanl1 (+) (map textWeight parts)
    chunks  = filter (T.any C.isSpace . fst) (zip parts weights)

    -- Stateful monadic computation which modifies spaces
    -- assigned to individual segments.
    onSent = mapM onWord
    onWord seg = do
        n <- addWeight seg
        s <- popSpace n
        let word' = (word seg) { space = s }
        return $ seg { word = word' }

    -- Add weight of the segment to the current weight.
    addWeight seg = S.state $ \(n, xs) ->
        let m = n + segWeight seg
        in (m, (m, xs))

    -- Pop space from the stack if its weight is lower than
    -- the current one.
    popSpace n = fmap (maybe None id) . M.runMaybeT $ do
        spaces  <- lift $ S.gets snd
        (sp, m) <- liftMaybe $ maybeHead spaces
        guard $ m < n
        lift $ S.modify $ \(n', xs) -> (n', tail xs)
        return $ toSpace sp
    liftMaybe = M.MaybeT . return
    maybeHead xs = case xs of
        (x:_)   -> Just x
        []      -> Nothing

    -- Parse strings representation of a Space.
    toSpace x
        | has '\n'  = NewLine 
        | has ' '   = Space 
        | otherwise = None
        where has c = maybe False (const True) (T.find (==c) x)


----------------------------
-- Pool
----------------------------


-- | A pool of Maca instances.
newtype MacaPool = MacaPool (Chan Maca)


-- | Run Maca server.
newMacaPool
    :: Int          -- ^ Number of Maca instances
    -> IO MacaPool
newMacaPool n = do
    chan  <- newChan
    macas <- replicateM n newMaca
    writeList2Chan chan macas
    return $ MacaPool chan


popMaca :: MacaPool -> IO Maca
popMaca (MacaPool c) = readChan c


putMaca :: Maca -> MacaPool -> IO ()
putMaca x (MacaPool c) = writeChan c x


-- | Analyse paragraph with Maca.  The function is thread-safe.  As a
-- pre-processing step, all non-printable characters are removed from
-- the input (based on empirical observations, Maca behaves likewise).
macaPar :: MacaPool -> T.Text -> IO [Sent Tag]
macaPar pool par0 = do
    let par = T.filter C.isPrint par0
    maca <- popMaca pool
    doMacaPar maca par `finally` putMaca maca pool


------------------------------------------------------------
-- Weight: a number of non-space characters
------------------------------------------------------------


-- | A weight of a text.
textWeight :: T.Text -> Int
-- textWeight = T.length . T.filter C.isAlphaNum
textWeight = T.length . T.filter (not . C.isSpace)
-- textWeight = T.length . T.filter ((&&) <$> not . C.isSpace <*> C.isPrint)


-- | A weight of a segment.
segWeight :: Seg t -> Int
segWeight = textWeight . orth . word


-- | A weight of a sentence.
sentWeight :: Sent t -> Int
sentWeight = sum . map segWeight
