{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simple format for morphosyntax representation which
-- assumes that all tags have a textual representation
-- with no spaces within and that one of the tags indicates
-- unknown words.

module NLP.Concraft.Polish.Format.Plain
(
-- * Parsing
  parsePlain
, parsePara
, parseSent

-- * Printing
, ShowCfg (..)
, showPlain
, showPara
, showSent
) where

import           Data.Monoid (Monoid, mappend, mconcat)
import           Data.Maybe (catMaybes)
import           Data.List (groupBy)
import           Data.String (IsString)
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.Text.Lazy.Read as R
import           Text.Printf (printf)

import qualified NLP.Concraft.Morphosyntax as X
import           NLP.Concraft.Polish.Morphosyntax

-- | Parse the text in the plain format.
parsePlain :: L.Text -> [[Sent Tag]]
parsePlain =
    map parsePara' . groupBy f . L.splitOn "\n\n"
  where
    f _ xs = case L.uncons xs of
        Nothing     -> False
        Just (x, _) -> not (C.isSpace x)

-- | Parse the paragraph in the plain format.
parsePara :: L.Text -> [Sent Tag]
parsePara = parsePara' . L.splitOn "\n\n"

-- | Parse paragraph already divided into sentence chunks.
parsePara' :: [L.Text] -> [Sent Tag]
parsePara' = map (parseSent . L.strip) . filter (not.isEmpty)

-- | Identify empty chunks of text.
isEmpty :: L.Text -> Bool
isEmpty = L.all C.isSpace

-- | Parse the sentence in the plain format.
parseSent :: L.Text -> Sent Tag
parseSent
    = map parseWord
    . groupBy (\_ x -> cond x)
    . L.lines
  where
    cond = ("\t" `L.isPrefixOf`)

parseWord :: [L.Text] -> Seg Tag
parseWord xs = Seg
    (Word _orth _space _known)
    _interps
  where
    (_orth, _space) = parseHeader (head xs)
    ys          = map parseInterp (tail xs)
    _known      = not (Nothing `elem` ys)
    -- _interps    = M.fromListWith max (catMaybes ys)
    _interps    = X.mkWMap $ catMaybes ys

parseInterp :: L.Text -> Maybe (Interp Tag, Double)
parseInterp =
    doIt . tail . L.splitOn "\t"
  where
    doIt [form, tag]
        | tag == ign    = Nothing
        | otherwise     = Just $
            (mkInterp form tag, 0)
    doIt [form, tag, "disamb"] =
        Just (mkInterp form tag, 1)
    doIt [form, tag, weight] = case R.double weight of
        Left er -> error $ "parseInterp (weight):" ++ show er
        Right w -> Just (mkInterp form tag, fst w)
    doIt xs = error $ "parseInterp: " ++ show xs
    mkInterp form tag = Interp (L.toStrict form) (L.toStrict tag)

parseHeader :: L.Text -> (T.Text, Space)
parseHeader xs =
    let [_orth, space] = L.splitOn "\t" xs
    in  (L.toStrict _orth, parseSpace space)

-- TODO: Should we represent newlines and spaces in the `Space` data type?
parseSpace :: L.Text -> Space
parseSpace "none"    = None
parseSpace "space"   = Space
parseSpace "spaces"  = Space	-- Multiple spaces
parseSpace "newline" = NewLine
parseSpace "newlines" = NewLine -- Multiple newlines
parseSpace xs        = error ("parseSpace: " ++ L.unpack xs)

-----------
-- Printing
-----------

-- | Printing configuration.
data ShowCfg = ShowCfg {
    -- | Print weights instead of 'disamb' tags.
      showWsCfg :: Bool }

-- | Show the plain data.
showPlain :: ShowCfg ->[[Sent Tag]] -> L.Text
showPlain cfg =
    L.intercalate "\n" . map (showPara cfg)

-- | Show the paragraph.
showPara :: ShowCfg -> [Sent Tag] -> L.Text
showPara cfg = L.toLazyText . mconcat . map (\xs -> buildSent cfg xs <> "\n")

-- | Show the sentence.
showSent :: ShowCfg -> Sent Tag -> L.Text
showSent cfg xs = L.toLazyText $ buildSent cfg xs

buildSent :: ShowCfg -> Sent Tag -> L.Builder
buildSent cfg = mconcat . map (buildWord cfg)

buildWord :: ShowCfg -> Seg Tag -> L.Builder
buildWord cfg Seg{..}
    =  L.fromText orth  <> "\t"
    <> buildSpace space <> "\n"
    <> buildKnown orth known
    <> buildInterps cfg interps
    where Word{..} = word

buildInterps :: ShowCfg -> X.WMap (Interp Tag) -> L.Builder
buildInterps ShowCfg{..} interps = mconcat
    [ "\t" <> buildBase interp <>
      "\t" <> buildTag interp  <> buildDmb dmb
    | (interp, dmb) <- M.toList (X.unWMap interps) ]
  where
    buildTag  = L.fromText . tag
    buildBase = L.fromText . base
    buildDmb  = case showWsCfg of
        True    -> \x -> between "\t" "\n"
            $ L.fromString
            $ printf "%.3f" x
        False   -> \x -> if x > 0
            then "\tdisamb\n"
            else "\n"
    between x y z = x <> z <> y

buildSpace :: Space -> L.Builder
buildSpace None     = "none"
buildSpace Space    = "space"
buildSpace NewLine  = "newline"

buildKnown :: T.Text -> Bool -> L.Builder
buildKnown _ True = ""
buildKnown lemma False
    =  "\t" <> L.fromText lemma
    <> "\t" <> L.fromText ign
    <> "\n"


-----------
-- Utils
-----------


-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- | Tag which indicates unknown words.
ign :: IsString a => a
ign = "ign"
{-# INLINE ign #-}
