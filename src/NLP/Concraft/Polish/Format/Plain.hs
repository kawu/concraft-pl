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
, parsePlainO
, parsePar
, parseSent

-- * Printing
, showPlain
, showPar
, showSent
) where

import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import           NLP.Concraft.Polish.Morphosyntax

noneBase :: T.Text
noneBase = "None"

-- | Parse the text in the plain format given the /oov/ tag.
-- Original sentences will be restored using the `withOrig`
-- function.  Plain format doesn't preserve original,
-- textual representation of individual sentences.
parsePlainO :: Tag -> L.Text -> [[SentO Tag]]
parsePlainO ign = map (map withOrig) . parsePlain ign

-- | Parse the text in the plain format given the /oov/ tag.
parsePlain :: Tag -> L.Text -> [[Sent Tag]]
parsePlain ign = map (parsePar ign) . init . L.splitOn "\n\n\n"

parsePar:: Tag -> L.Text -> [Sent Tag]
parsePar ign = map (parseSent ign) . init . L.splitOn "\n\n"

-- | Parse the sentence in the plain format given the /oov/ tag.
parseSent :: Tag -> L.Text -> Sent Tag
parseSent ign
    = map (parseWord ignL)
    . groupBy (\_ x -> cond x)
    . L.lines
  where
    cond = ("\t" `L.isPrefixOf`)
    ignL = L.fromStrict ign

parseWord :: L.Text -> [L.Text] -> Seg Tag
parseWord ign xs = Seg
    (Word _orth _space _known)
    _interps
  where
    (_orth, _space) = parseHeader (head xs)
    ys          = map (parseInterp ign) (tail xs)
    _known      = not (Nothing `elem` ys)
    _interps    = M.fromListWith max (catMaybes ys)

parseInterp :: L.Text -> L.Text -> Maybe (Interp Tag, Bool)
parseInterp ign =
    doIt . tail . L.splitOn "\t"
  where
    doIt [form, tag]
        | tag == ign    = Nothing
        | otherwise     = Just $
            (mkInterp form tag, False)
    doIt [form, tag, "disamb"] = Just $
        (mkInterp form tag, True)
    doIt xs = error $ "parseInterp: " ++ show xs
    mkInterp form tag
        | formS == noneBase = Interp Nothing tagS
        | otherwise         = Interp (Just formS) tagS
      where
        formS   = L.toStrict form
        tagS    = L.toStrict tag

parseHeader :: L.Text -> (T.Text, Space)
parseHeader xs =
    let [_orth, space] = L.splitOn "\t" xs
    in  (L.toStrict _orth, parseSpace space)

parseSpace :: L.Text -> Space
parseSpace "none"    = None
parseSpace "space"   = Space
parseSpace "spaces"  = Space	-- Is it not a Maca bug?
parseSpace "newline" = NewLine
parseSpace "newlines" = NewLine -- TODO: Remove this temporary fix
parseSpace xs        = error ("parseSpace: " ++ L.unpack xs)

-----------
-- Printing
-----------

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

-- | Show the plain data.
showPlain :: Tag -> [[Sent Tag]] -> L.Text
showPlain ign =
    L.intercalate "\n" . map (showPar ign)

-- | Show the paragraph.
showPar :: Tag -> [Sent Tag] -> L.Text
showPar ign =
    L.toLazyText . mconcat  . map (\xs -> buildSent ign xs <> "\n")

-- | Show the sentence.
showSent :: Tag -> Sent Tag -> L.Text
showSent ign xs = L.toLazyText $ buildSent ign xs

buildSent :: Tag -> Sent Tag -> L.Builder
buildSent ign = mconcat . map (buildWord ign)

buildWord :: Tag -> Seg Tag -> L.Builder
buildWord ign Seg{..}
    =  L.fromText orth  <> "\t"
    <> buildSpace space <> "\n"
    <> buildKnown ign known
    <> buildInterps (M.toList interps)
    where Word{..} = word

buildInterps :: [(Interp Tag, Bool)] -> L.Builder
buildInterps interps = mconcat
    [ "\t" <> buildBase interp <>
      "\t" <> buildTag  interp <>
      if dmb
        then "\tdisamb\n"
        else "\n"
    | (interp, dmb) <- interps ]
  where
    buildTag    = L.fromText . tag
    buildBase x = case base x of
        Just b  -> L.fromText b
        Nothing -> L.fromText noneBase

buildSpace :: Space -> L.Builder
buildSpace None     = "none"
buildSpace Space    = "space"
buildSpace NewLine  = "newline"

buildKnown :: Tag -> Bool -> L.Builder
buildKnown _   True     = ""
buildKnown ign False    =  "\t" <> L.fromText noneBase
                        <> "\t" <> L.fromText ign <> "\n"
