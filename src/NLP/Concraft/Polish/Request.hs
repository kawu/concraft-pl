{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module NLP.Concraft.Polish.Request
(
-- * Request 
  Request (..)
, Config (..)
-- ** Short
, Short (..)
, short
-- ** Long
, Long (..)
, long
) where


import           Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.LazyIO as LazyIO
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Binary as B

import           NLP.Concraft.Polish
import           NLP.Concraft.Polish.Maca
import           NLP.Concraft.Polish.Morphosyntax hiding (tag)


-------------------------------------------------
-- Configuration
-------------------------------------------------


-- | A request with configuration.
data Request t = Request {
    -- | The actuall request.
      rqBody    :: t
    -- | Request configuration.
    , rqConf    :: Config }


instance B.Binary t => B.Binary (Request t)  where
    put Request{..} = B.put rqBody >> B.put rqConf
    get = Request <$> B.get <*> B.get


-- | Tagging configuration.
newtype Config = Config {
    -- | Tag with marginal probabilities.
      tagProbs  :: Bool
    } deriving (B.Binary)


-------------------------------------------------
-- Short request
-------------------------------------------------


-- | A short request.
data Short
    = Short T.Text
    | Par [Sent Tag]


instance B.Binary Short where
    put (Short x) = B.putWord8 0 >> B.put x
    put (Par x)   = B.putWord8 1 >> B.put x
    get = B.getWord8 >>= \x -> case x of
        0   -> Short <$> B.get
        _   -> Par   <$> B.get


-- | Process the short request.
short :: MacaPool -> Concraft -> Request Short -> IO [Sent Tag]
short pool concraft Request{..} = case rqBody of
    Short x -> map (tagit concraft) <$> macaPar pool x
    Par x   -> return $ map (tagit concraft) x
  where
    tagit = if tagProbs rqConf then marginals else tag


-------------------------------------------------
-- Long request
-------------------------------------------------


-- | A request to parse a long text.
data Long
    = Long L.Text
    | Doc [[Sent Tag]]


instance B.Binary Long where
    put (Long x) = B.putWord8 0 >> B.put x
    put (Doc x)  = B.putWord8 1 >> B.put x
    get = B.getWord8 >>= \x -> case x of
        0   -> Long <$> B.get
        _   -> Doc  <$> B.get


-- | Process the long request given the processor for the
-- short request. 
long :: (Request Short -> IO a) -> Request Long -> IO [a]
long handler Request{..} = case rqBody of
    Long inp ->
          LazyIO.mapM f . map L.unlines
        . Split.splitWhen (L.all Char.isSpace)
        . L.lines $ inp
    Doc inp -> LazyIO.mapM g inp
  where
    f x = handler . r $ Short $ L.toStrict x
    g x = handler . r $ Par x
    r x = Request {rqBody = x, rqConf = rqConf}
