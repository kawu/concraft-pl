{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}


-- | Concraft (DAG) web server.


module NLP.Concraft.Polish.DAG.Server
  (
    -- * Types
    ServerCfg(..)
  , ClientCfg(..)
  , Request(..)
  , Answer(..)

    -- * Server
  , runServer

    -- * Client
  , sendRequest
  ) where


import           Control.Monad.IO.Class (liftIO)

import           GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Network.HTTP.Types (badRequest400)
import qualified Web.Scotty as W
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Network.Wreq as Wreq
import           Control.Lens ((^?))

import qualified NLP.Concraft.Polish.DAGSeg as Pol
import qualified NLP.Concraft.Polish.DAG.Format.Base as DB


---------------------------------------
-- Types
---------------------------------------


type Concraft = Pol.Concraft Pol.Tag


data Request = Request
  { dag :: T.Text
  } deriving (Generic)

instance A.FromJSON Request
instance A.ToJSON Request


data Answer = Answer
  { dag :: T.Text
  } deriving (Generic)

instance A.ToJSON Answer
instance A.FromJSON Answer


data ServerCfg = ServerCfg
  { concraft :: Concraft
  , annoCfg :: Pol.AnnoConf
  , showCfg :: DB.ShowCfg
  }


---------------------------------------
-- Server
---------------------------------------


serverApp :: ServerCfg -> W.ScottyM ()
serverApp env = do
  W.post "/parse" $ parse env
  W.post "/parse" parseFailure


parse :: ServerCfg -> W.ActionM ()
parse ServerCfg{..} =  flip W.rescue (const W.next) $ do
  Request{..} <- W.jsonData
  let inp = DB.parseData (L.fromStrict dag)
      out = Pol.annoAll annoCfg concraft <$> inp
      dagStr = DB.showData showCfg out
      ans = Answer { dag = L.toStrict dagStr }
  -- W.json $ A.object [ "dag" .= dagStr ]
  W.json $ A.toJSON ans


parseFailure :: W.ActionM ()
parseFailure = do
  W.json $ A.object
    [ "error" .= ("Invalid request" :: T.Text) ]
  W.status badRequest400


runServer
  :: ServerCfg
  -> Int -- ^ Port
  -> IO ()
runServer env port = W.scotty port (serverApp env)


---------------------------------------
-- Client
---------------------------------------


data ClientCfg = ClientCfg
  { serverAddr :: String
  }


sendRequest
  :: ClientCfg
  -> Request
  -> IO (Maybe Answer)
sendRequest ClientCfg{..} req = do
  let json = A.toJSON req
  r <- Wreq.post serverAddr json
  let result = A.decode =<< r ^? Wreq.responseBody
  return result
