{-# LANGUAGE OverloadedStrings #-}


module NLP.Concraft.Polish
(
-- * Model
  C.Concraft
, C.saveModel
, C.loadModel

-- * Tagging
, tag
, tag'
, tagSent

-- * Training
, train
) where


import           System.IO.Unsafe (unsafeInterleaveIO)
import           Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD as SGD

import qualified NLP.Concraft.Schema as S
import           NLP.Concraft.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D
import qualified NLP.Concraft as C

import           NLP.Concraft.Polish.Morphosyntax hiding (tag)
import           NLP.Concraft.Polish.Maca


-------------------------------------------------
-- Default configuration
-------------------------------------------------


-- | Default configuration for the guessing observation schema.
guessConfDefault :: SchemaConf
guessConfDefault = S.nullConf
    { lowPrefixesC  = entryWith [1, 2]      [0]
    , lowSuffixesC  = entryWith [1, 2]      [0]
    , knownC        = entry                 [0]
    , begPackedC    = entry                 [0] }


-- | Default configuration for the guessing observation schema.
disambConfDefault :: SchemaConf
disambConfDefault = S.nullConf
    { lowOrthC      = entry                         [-1, 0, 1]
    , lowPrefixesC  = oov $ entryWith [1, 2, 3]     [0]
    , lowSuffixesC  = oov $ entryWith [1, 2, 3]     [0]
    , begPackedC    = oov $ entry                   [0] }
  where
    oov (Just body) = Just $ body { S.oovOnly = True }
    oov Nothing     = Nothing


-- | Default tiered tagging configuration.
tiersDefault :: [D.Tier]
tiersDefault =
    [tier1, tier2]
  where
    tier1 = D.Tier True $ S.fromList ["cas", "per"]
    tier2 = D.Tier False $ S.fromList
        [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
        , "acn", "ppr", "agg", "vlc", "dot" ]


-------------------------------------------------
-- Tagging
-------------------------------------------------


-- | Perform morphological tagging on the input text.
tag :: MacaPool -> C.Concraft -> T.Text -> IO [Sent Tag]
tag pool concraft inp = map (tagSent concraft) <$> macaPar pool inp


-- | An alernative tagging function which interprets
-- empty lines as paragraph ending markers.
-- The function uses lazy IO so it can be used to
-- analyse large chunks of data.
tag' :: MacaPool -> C.Concraft -> L.Text -> IO [[Sent Tag]]
tag' pool concraft
    = lazyMapM (tag pool concraft . L.toStrict)
    . map L.strip
    . L.splitOn "\n\n"


-- | Tag an already analysed sentence.
tagSent :: C.Concraft -> Sent Tag -> Sent Tag
tagSent concraft inp =
    let tagset = C.tagset concraft
        packed = packSentTag tagset inp
        xs = C.tag concraft packed
    in  embedSent inp $ map (P.showTag tagset) xs


lazyMapM :: (a -> IO b) -> [a] -> IO [b]
lazyMapM f (x:xs) = do
    y <- f x
    ys <- unsafeInterleaveIO $ lazyMapM f xs
    return (y:ys)
lazyMapM _ [] = return []


-------------------------------------------------
-- Training
-------------------------------------------------


-- | Train concraft model.
-- TODO: It should be possible to supply the two training procedures with
-- different SGD arguments.
train
    :: SGD.SgdArgs      -- ^ SGD parameters
    -> P.Tagset         -- ^ Tagset
    -> Int              -- ^ Numer of guessed tags for each word 
    -> [SentO Tag]      -- ^ Training data
    -> Maybe [SentO Tag] -- ^ Maybe evaluation data
    -> IO C.Concraft
train sgdArgs tagset guessNum train0 eval0 = do
    pool <- newMacaPool 1
    let guessConf  = G.TrainConf guessConfDefault sgdArgs
        disambConf = D.TrainConf tiersDefault disambConfDefault sgdArgs
        ana = fmap (packSentTag tagset . concat) . macaPar pool . L.toStrict
    C.train tagset ana guessNum guessConf disambConf
        (map (packSentTagO tagset)     train0)
        (map (packSentTagO tagset) <$> eval0)
