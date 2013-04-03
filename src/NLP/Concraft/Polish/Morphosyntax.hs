-- | Morphosyntax data layer in Polish.

module NLP.Concraft.Polish.Morphosyntax
( 
-- * Token 
  Token (..)
, Interp (..)
, Space (..)
, Tag

-- * Sentence
, Sent

-- * Conversion
-- , baseTok
) where

import qualified Data.Map as M
import qualified Data.Text as T

import qualified NLP.Concraft.Morphosyntax as X

-- | A textual representation of a morphosyntactic tag.
type Tag = T.Text

-- | A token.
data Token t = Token
    { orth      :: T.Text
    , space     :: Space
    , known     :: Bool
    -- | Interpretations of the token, each interpretation annotated
    -- with a /disamb/ Boolean value (if 'True', the interpretation
    -- is correct within the context).
    , interps   :: M.Map (Interp t) Bool }
    deriving (Show, Eq, Ord)

instance X.HasOrth (Token t) where
    orth = orth

instance X.HasOOV (Token t) where
    oov = not.known
    
-- | An interpretation.
-- TODO: Should we allow `base` to be `Nothing`?
data Interp t = Interp
    { base  :: Maybe T.Text
    , tag   :: t }
    deriving (Show, Eq, Ord)

-- | No space, space or newline.
-- TODO: Perhaps we should use a bit more informative data type.
data Space
    = None
    | Space
    | NewLine
    deriving (Show, Eq, Ord)

-- | A sentence.
type Sent t = [Token t]

---------------------------
-- Conversion
---------------------------

-- -- | Convert a token to a morphosyntax segment.
-- baseTok :: Ord a => Token a -> X.Seg a
-- baseTok tok = X.Seg
--     { X.orth       = orth tok
--     , X.tags       = X.mkWMap
--         [ (tag x, if disamb then 1 else 0)
--         | (x, disamb) <- M.toList (interps tok) ]
--     , X.oov        = not (known tok) }
