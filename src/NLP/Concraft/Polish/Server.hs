{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft.Polish.Server
( 
-- * Server
  runConcraftServer

-- * Client
, submit
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forever, void)
import           Control.Concurrent (forkIO)
import           System.IO (Handle, hFlush)
import qualified Network as N
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS

import           NLP.Concraft.Polish.Morphosyntax hiding (tag)
import           NLP.Concraft.Polish.Maca
import qualified NLP.Concraft.Polish as C
import qualified NLP.Concraft.Polish.Request as R


-------------------------------------------------
-- Server
-------------------------------------------------


-- | Run a Concraft server on a given port.
runConcraftServer :: MacaPool -> C.Concraft -> N.PortID -> IO ()
runConcraftServer pool concraft port = N.withSocketsDo $ do
    sock <- N.listenOn port
    forever $ sockHandler pool concraft sock


-- | Read and process short requests from the socket.
sockHandler :: MacaPool -> C.Concraft -> N.Socket -> IO ()
sockHandler pool concraft sock = do
    (handle, _, _) <- N.accept sock
    -- putStrLn "Connection established"
    void $ forkIO $ do
        -- putStrLn "Waiting for input..."
        inp <- recvMsg handle
        -- putStr "> " >> T.putStrLn inp
        out <- R.short pool concraft inp
        -- putStr "No. of sentences: " >> print (length out)
        sendMsg handle out


-------------------------------------------------
-- Client
-------------------------------------------------


-- | Submit the given request.
submit :: N.HostName -> N.PortID -> R.Request R.Short -> IO [Sent Tag]
submit host port inp = do
    handle <- N.connectTo host port
    -- putStrLn "Connection established"
    -- putStr "Send request: " >> T.putStrLn inp
    sendMsg handle inp
    recvMsg handle


-------------------------------------------------
-- Communication
-------------------------------------------------


sendMsg :: B.Binary a => Handle -> a -> IO ()
sendMsg h msg = do
    let x = B.encode msg
        n = fromIntegral $ BS.length x
    sendInt h n
    BS.hPut h x
    hFlush h


recvMsg :: B.Binary a => Handle -> IO a
recvMsg h = do
    n <- recvInt h
    B.decode <$> BS.hGet h n


sendInt :: Handle -> Int -> IO ()
sendInt h x = BS.hPut h (B.encode x)


recvInt :: Handle -> IO Int
recvInt h = B.decode <$> BS.hGet h 8
    

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
