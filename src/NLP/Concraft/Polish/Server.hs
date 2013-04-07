{-# LANGUAGE OverloadedStrings #-}


module NLP.Concraft.Polish.Server
( 
-- * Server
  runConcraftServer

-- * Client
, tag
, tag'
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forever, void)
import           Control.Concurrent (forkIO)
import           System.IO.Unsafe (unsafeInterleaveIO)
import qualified Network as N
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L

import           NLP.Concraft.Polish.Morphosyntax hiding (tag)
import           NLP.Concraft.Polish.Maca
import qualified NLP.Concraft.Polish as C


-------------------------------------------------
-- Server
-------------------------------------------------


-- | Run a Concraft server on a given port.
runConcraftServer :: Maca -> C.Concraft -> N.PortID -> IO ()
runConcraftServer maca concraft port = N.withSocketsDo $ do
    sock <- N.listenOn port
    putStrLn $ "Listening on " ++ show port
    forever $ sockHandler maca concraft sock


sockHandler :: Maca -> C.Concraft -> N.Socket -> IO ()
sockHandler maca concraft sock = do
    (handle, _, _) <- N.accept sock
    -- BlockBuffering is a default buffering, according to docs.
    -- hSetBuffering handle $ BlockBuffering Nothing
    void $ forkIO $ do
        inp <- T.hGetContents handle
        out <- C.tag maca concraft inp
        BS.hPut handle $ B.encode out


-------------------------------------------------
-- Client
-------------------------------------------------


-- | Perform morphological tagging on the input text.
tag :: N.HostName -> N.PortID -> T.Text -> IO [Sent Tag]
tag host port inp = do
    handle <- N.connectTo host port
    T.hPutStr handle inp
    B.decode <$> BS.hGetContents handle


-- | An alernative tagging function which interprets
-- empty lines as paragraph ending markers.
-- The function uses lazy IO so it can be used to
-- analyse large chunks of data.
tag' :: N.HostName -> N.PortID -> L.Text -> IO [[Sent Tag]]
tag' host port
    = lazyMapM (tag host port . L.toStrict)
    . map L.strip
    . L.splitOn "\n\n"


lazyMapM :: (a -> IO b) -> [a] -> IO [b]
lazyMapM f (x:xs) = do
    y <- f x
    ys <- unsafeInterleaveIO $ lazyMapM f xs
    return (y:ys)
lazyMapM _ [] = return []


-- -------------------------------------------------
-- -- Stream binary encoding
-- -------------------------------------------------
-- 
-- 
-- newtype Stream a = Stream { unstream :: [a] }
-- 
-- 
-- instance B.Binary a => B.Binary (Stream a) where
--     put (Stream [])     = B.putWord8 0
--     put (Stream (x:xs)) = B.putWord8 1 >> B.put x >> B.put (Stream xs)
--     get = error "use lazyDecodeStream insted"
-- 
-- 
-- getMaybe :: B.Binary a => B.Get (Maybe a)
-- getMaybe = do
--     t <- B.getWord8
--     case t of
--         0 -> return Nothing
--         _ -> fmap Just B.get
-- 
-- 
-- step :: B.Binary a => (ByteString, Int64) -> Maybe (a, (ByteString, Int64))
-- step (xs, offset) = case B.runGetState getMaybe xs offset of
--     (Just v, ys, newOffset) -> Just (v, (ys, newOffset))
--     _                       -> Nothing
-- 
-- 
-- lazyDecodeList :: B.Binary a => ByteString -> [a]
-- lazyDecodeList xs = unfoldr step (xs, 0)
-- 
-- 
-- lazyDecodeStream :: B.Binary a => ByteString -> Stream a
-- lazyDecodeStream = Stream . lazyDecodeList
