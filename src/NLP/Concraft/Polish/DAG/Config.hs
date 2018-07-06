{-# LANGUAGE DeriveGeneric #-}


module NLP.Concraft.Polish.DAG.Config
  ( Config(..)
  ) where


import           Dhall
import qualified Data.Aeson as JSON

import qualified NLP.Concraft.Polish.DAG.Config.Disamb as D


-- | Tagging configuration (everything apart tagset).
data Config = Config
  { disambCfg :: D.DisambCfg
  -- , guessCfg :: G.GuessCfg
  } deriving (Generic, Show)


instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
