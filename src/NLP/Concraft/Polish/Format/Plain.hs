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

import           NLP.Concraft.Polish.Morphosyntax

noneBase :: T.Text
noneBase = "None"

-- | Parse the text in the plain format.
-- TODO: Handling spaces between paragraphs should be smarter.
-- Right now, if a paragraph is very long, the underlying sentences
-- will not be read until the entire paragraph bytestring is read
-- into the program memory.
parsePlain :: L.Text -> [[Sent Tag]]
parsePlain
    = map (parsePara . L.strip)
    . filter (not.isEmpty) . L.splitOn "\n\n\n"

-- | Parse the paragraph in the plain format.
parsePara :: L.Text -> [Sent Tag]
parsePara
    = map (parseSent . L.strip)
    . filter (not.isEmpty) . L.splitOn "\n\n"

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
    _interps    = M.fromListWith max (catMaybes ys)

parseInterp :: L.Text -> Maybe (Interp Tag, Bool)
parseInterp =
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

-- | Show the plain data.
showPlain :: [[Sent Tag]] -> L.Text
showPlain =
    L.intercalate "\n" . map showPara

-- | Show the paragraph.
showPara :: [Sent Tag] -> L.Text
showPara = L.toLazyText . mconcat  . map (\xs -> buildSent xs <> "\n")

-- | Show the sentence.
showSent :: Sent Tag -> L.Text
showSent xs = L.toLazyText $ buildSent xs

buildSent :: Sent Tag -> L.Builder
buildSent = mconcat . map buildWord

buildWord :: Seg Tag -> L.Builder
buildWord Seg{..}
    =  L.fromText orth  <> "\t"
    <> buildSpace space <> "\n"
    <> buildKnown known
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

buildKnown :: Bool -> L.Builder
buildKnown True  = ""
buildKnown False =  "\t" <> L.fromText noneBase
                         <> "\t" <> L.fromText ign <> "\n"


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
