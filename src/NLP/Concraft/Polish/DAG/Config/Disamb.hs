{-# LANGUAGE DeriveGeneric #-}


module NLP.Concraft.Polish.DAG.Config.Disamb
  ( DisambCfg (..)
  , TierCfg (..)
  , Set(..)
  ) where


import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as T

import           Dhall
import qualified Data.Aeson as JSON


-- | Disamb module configuration.
data DisambCfg = DisambCfg
  { tiersCfg :: [TierCfg]
  } deriving (Generic, Show, Eq, Ord)


-- | Disamb tiers configuration.
data TierCfg = TierCfg
  { withPos :: Bool
  , withEos :: Bool
  , withAtts :: Set T.Text
  } deriving (Generic, Show, Eq, Ord)


instance Interpret DisambCfg

instance JSON.FromJSON DisambCfg
instance JSON.ToJSON DisambCfg where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


instance Interpret TierCfg

instance JSON.FromJSON TierCfg
instance JSON.ToJSON TierCfg where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


------------------------------
-- Set
------------------------------


newtype Set a = Set {unSet :: S.Set a}
  deriving (Generic, Show, Eq, Ord)

instance (Ord a, Interpret a) => Interpret (Set a) where
    autoWith = fmap
      (fmap $ Set . S.fromList . V.toList)
      autoWith

instance (Ord a, JSON.FromJSON a) => JSON.FromJSON (Set a)
instance JSON.ToJSON a => JSON.ToJSON (Set a) where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
