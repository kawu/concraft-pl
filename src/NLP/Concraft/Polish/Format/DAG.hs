{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft.Polish.Format.DAG
(
-- * Printing
  ShowCfg (..)
, showSent
, showData
) where


import           Data.Monoid (mconcat, mappend)
import qualified Data.Map as M
import           Data.List (intersperse)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import           Text.Printf (printf)

import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG
import qualified NLP.Concraft.Morphosyntax.DAG as X
import           NLP.Concraft.Polish.Morphosyntax.DAG


-- | Printing configuration.
data ShowCfg = ShowCfg


-- | Show the given sentence.
showData :: ShowCfg -> [Sent Tag] -> L.Text
showData cfg
  = L.toLazyText
  . mconcat
  . intersperse "\n"
  . map (buildSent cfg)

-- | Show the given sentence.
showSent :: ShowCfg -> Sent Tag -> L.Text
showSent cfg = L.toLazyText . buildSent cfg

buildSent :: ShowCfg -> Sent Tag -> L.Builder
buildSent _cfg dag = finalize $ do
  edgeID <- DAG.dagEdges dag
  let tailNode = DAG.begsWith edgeID dag
      headNode = DAG.endsWith edgeID dag
      Edge{..} = DAG.edgeLabel edgeID dag
  (Interp{..}, weight) <- M.toList (X.unWMap interps)
  return . mconcat $ intersperse "\t"
    [ buildNode tailNode
    , buildNode headNode
    , L.fromText (orth word)
    , L.fromText base
    , L.fromText tag
    , buildDmb weight ]
  where
    finalize = (`mappend` "\n") . mconcat . intersperse "\n"
    buildNode (DAG.NodeID i) = L.fromString (show i)
    buildDmb = L.fromString . printf "%.3f"
    -- buildDmb = between "\t" "\n" . L.fromString . printf "%.3f"
    -- between x y z = x <> z <> y


-----------
-- Utils
-----------


-- -- | An infix synonym for 'mappend'.
-- (<>) :: Monoid m => m -> m -> m
-- (<>) = mappend
-- {-# INLINE (<>) #-}
