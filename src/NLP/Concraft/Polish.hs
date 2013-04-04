{-# LANGUAGE OverloadedStrings #-}

module NLP.Concraft.Polish
(
-- * Types
  C.Concraft

-- * Tagging
, tag
, tagSent

-- * Training
, train
, trainNoAna

-- * Default schemas
, guessConfDefault
, disambConfDefault 

-- * Misc
, macalyse
, ign
) where

import           Control.Applicative ((<$>))
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Process.Text.Lazy as Proc
import           Data.String (IsString)
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
import qualified NLP.Concraft.Polish.Format.Plain as Plain

-- | Tag which indicates unknown words. 
ign :: IsString a => a
ign = "ign"
{-# INLINE ign #-}

-------------------------------------------------
-- Analysis with external Maca tool
-------------------------------------------------

-- | Use Maca to analyse the input text.
-- TODO: Is it even lazy?
macalyse :: L.Text -> [[Sent Tag]]
macalyse inp = unsafePerformIO $ do
    let args = ["-q", "morfeusz-nkjp-official", "-o", "plain", "-s"]
    (_exitCode, out, _) <- Proc.readProcessWithExitCode "maca-analyse" args inp
    return $ Plain.parsePlain ign out

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
tag :: C.Concraft -> L.Text -> [[Sent Tag]]
tag concraft = (map.map) (tagSent concraft) . macalyse

tagSent :: C.Concraft -> Sent Tag -> Sent Tag
tagSent concraft inp =
    let tagset = C.tagset concraft
        packed = packSentTag tagset inp
        xs = C.tag concraft packed
    in  embedSent inp $ map (P.showTag tagset) xs

----------------------------------------------
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
    let guessConf  = G.TrainConf guessConfDefault sgdArgs
        disambConf = D.TrainConf tiersDefault disambConfDefault sgdArgs
        maca = map (packSentTag tagset) . concat . macalyse
    C.train tagset maca guessNum guessConf disambConf
        (map (packSentTagO tagset)     train0)
        (map (packSentTagO tagset) <$> eval0)

-- | Train concraft model.
trainNoAna
    :: SGD.SgdArgs      -- ^ SGD parameters
    -> P.Tagset         -- ^ Tagset
    -> Int              -- ^ Numer of guessed tags for each word 
    -> [Sent Tag]       -- ^ Training data
    -> Maybe [Sent Tag] -- ^ Maybe evaluation data
    -> IO C.Concraft
trainNoAna sgdArgs tagset guessNum train0 eval0 = do
    let guessConf  = G.TrainConf guessConfDefault sgdArgs
        disambConf = D.TrainConf tiersDefault disambConfDefault sgdArgs
    C.trainNoAna tagset guessNum guessConf disambConf
        (map (packSentTag tagset)     train0)
        (map (packSentTag tagset) <$> eval0)
