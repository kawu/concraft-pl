{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}


module NLP.Concraft.Polish.DAG.Format.Base
(
-- * Printing
  ShowCfg (..)
, ProbType (..)
, showSent
, showData

-- * Parsing
, parseData
, parseSent
) where


import           Data.Monoid (mconcat, mappend)
import qualified Data.Map as M
import           Data.List (intersperse, groupBy)
import           Data.Maybe (listToMaybe)
import           Data.String (IsString)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import           Text.Printf (printf)

import qualified Data.DAG as DAG
-- import qualified Data.CRF.Chain1.Constrained.DAG.Dataset.Internal as DAG

import qualified NLP.Concraft.DAG.Morphosyntax as X
import qualified NLP.Concraft.Polish.DAG2 as C
import           NLP.Concraft.Polish.DAG2 (AnnoSent(..))
import qualified NLP.Concraft.Polish.Morphosyntax as I
import           NLP.Concraft.Polish.DAG.Morphosyntax


-----------------------------
-- Showing
-----------------------------


-- | Printing configuration.
data ShowCfg = ShowCfg
  { suppressProbs :: Bool
    -- ^ Do not show any probabilities
  , probType  :: ProbType
    -- ^ Which type of probabilities to show (unless suppressed)
  }


-- | Type of probabilities.
data ProbType
  = Marginals
    -- ^ Marginals of the disambiguation model
  | MaxProbs
    -- ^ Max probabilities of the disambiguation model
  | GuessedMarginals
    -- ^ Marginals of the guessing model
  deriving (Show, Eq, Ord, Enum, Typeable, Data)
-- Above, deriving Typeable and Data so that it can be easily parsed
-- for the command-line tool.


-- mkProbType :: ProbType -> Disamb.ProbType
-- mkProbType Marginals = Disamb.Marginals
-- mkProbType MaxProbs = Disamb.MaxProbs


-- | Show the given sentence.
showData :: ShowCfg -> [AnnoSent] -> L.Text
showData cfg
  = L.toLazyText
  . mconcat
  . intersperse "\n"
  . map (buildSent cfg)

-- | Show the given sentence.
showSent :: ShowCfg -> AnnoSent -> L.Text
showSent cfg = L.toLazyText . buildSent cfg

buildSent :: ShowCfg -> AnnoSent -> L.Builder
buildSent ShowCfg{..} AnnoSent{..} = finalize $ do
  let dag = guessSent
  edgeID <- DAG.dagEdges dag
  let tailNode = DAG.begsWith edgeID dag
      headNode = DAG.endsWith edgeID dag
      Edge{..} = DAG.edgeLabel edgeID dag
  interp <- map Just (M.toList (X.unWMap interps)) ++
            if known word then [] else [Nothing]
  return $ case interp of
    Just (Interp{..}, weight) ->
      mconcat $ intersperse "\t" $
        [ buildNode tailNode
        , buildNode headNode
        , L.fromText (orth word)
        , L.fromText base
        , L.fromText tag ] ++
        ( if suppressProbs then [] else
            [ case probType of
                Marginals ->
                  buildWeight $ tagWeightIn edgeID tag marginals
                MaxProbs ->
                  buildWeight $ tagWeightIn edgeID tag maxProbs
                GuessedMarginals ->
                  buildWeight weight ] ) ++
        [ buildBool $ tagLabelIn False edgeID tag disambs ]
    -- below, the case when the word is unknown
    Nothing ->
      mconcat $ intersperse "\t" $
        [ buildNode tailNode
        , buildNode headNode
        , L.fromText (orth word)
        , L.fromText "none"
        , L.fromText ign ] ++
        if suppressProbs then [] else [buildWeight 0]
  where
    finalize = (`mappend` "\n") . mconcat . intersperse "\n"
    buildNode (DAG.NodeID i) = L.fromString (show i)
    buildWeight = L.fromString . printf "%.3f"
    buildBool True = "disamb"
    buildBool False = ""
    -- buildDmb = between "\t" "\n" . L.fromString . printf "%.3f"
    -- between x y z = x <> z <> y

    tagWeightIn = tagLabelIn 0
    tagLabelIn def i x anno
      = maybe def (tagLabel def x) (DAG.maybeEdgeLabel i anno)
    tagLabel def x = maybe def id . M.lookup x


-----------------------------
-- Parsing
-----------------------------


-- | Parse the text in the DAG format.
parseData :: L.Text -> [Sent Tag]
parseData =
  map parseSent . filter realSent . L.splitOn "\n\n"
  where
    realSent = not . L.null


-- | Parse sentence in the DAG format.
parseSent :: L.Text -> Sent Tag
parseSent = fromRows . parseRows


data Row = Row
  { tailNode :: Int
  , headNode :: Int
  , orthForm :: T.Text
  , baseForm :: T.Text
  , theTag   :: Tag
  , tagProb  :: Double }


fromRows :: [Row] -> Sent Tag
fromRows =
  -- DAG.fromList' I.None . zip (repeat I.None) . getEdges
  DAG.mapN (const I.None) . DAG.fromEdgesUnsafe . getEdges
  where
    getEdges = map mkEdge . groupBy theSameEdge
    theSameEdge r1 r2
      =  tailNode r1 == tailNode r2
      && headNode r1 == headNode r2
    mkEdge [] = error "Format.Base.fromRows: empty list"
    mkEdge rows@(row0:_) = DAG.Edge
      { DAG.tailNode = DAG.NodeID $ tailNode row0
      , DAG.headNode = DAG.NodeID $ headNode row0
      , DAG.edLabel = edge }
      where
        edge = Edge
          { word = newWord
          , interps = newInterps }
        newWord = Word
          { orth = orthForm row0
          , known = not $ ign `elem` map theTag rows }
        newInterps = X.mkWMap
          [ (interp, tagProb)
          | Row{..} <- rows
          , not $ theTag == ign
          , let interp = Interp
                  { base = baseForm
                  , tag = theTag } ]


parseRows :: L.Text -> [Row]
parseRows = map parseRow . L.splitOn "\n"


parseRow :: L.Text -> Row
parseRow =
  doit . L.splitOn "\t"
  where
    doit [tlNode, hdNode, otForm, bsForm, tag, prob] = Row
      { tailNode = read $ L.unpack tlNode
      , headNode = read $ L.unpack hdNode
      , orthForm = L.toStrict otForm
      , baseForm = L.toStrict bsForm
      , theTag   = L.toStrict tag
      , tagProb  = read $ L.unpack prob }
    doit _ = error "parseRow: unexpected number of row cells"


-----------
-- Utils
-----------


-- -- | An infix synonym for 'mappend'.
-- (<>) :: Monoid m => m -> m -> m
-- (<>) = mappend
-- {-# INLINE (<>) #-}


-- | Tag which indicates unknown words.
ign :: IsString a => a
ign = "ign"
{-# INLINE ign #-}
