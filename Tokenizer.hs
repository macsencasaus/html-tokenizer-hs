{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tokenizer
-- Description : An HTML tokenizer library for web scraping
--
-- Created after reading Haskell in Haskell by cronokirby
-- Specifically : https://cronokirby.com/posts/2020/12/haskell-in-haskell-2/
--
-- Exposes two types: Token and TokenizerError
-- Exposes one function: tokenizer to convert bytestring to either error or list of tokens
module Tokenizer (Token (..), TokenizerError (..), tokenizer) where

import Control.Applicative
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower, toUpper)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)

-- * Code

-- ** Type definitions

-- | Tokenizer errors, represents errors that can occur
data TokenizerError
  = -- Encountered a character it wasn't expecting to see
    Unexpected Char
  | -- Encountered EOF before it was expected
    UnexpectedEOF
  deriving (Eq, Show)

-- | Helper function to produce correct error given a string
unexpected :: ByteString -> TokenizerError
unexpected str
  | B8.null str = UnexpectedEOF
  | otherwise = let c = B8.head str in Unexpected c

-- | A tokenizer takes an input string, and can consum part of that string to return a result, or fail
newtype Tokenizer a = Tokenizer
  { runTokenizer :: ByteString -> Either TokenizerError (a, ByteString)
  }

-- | Maps over the resulting value to change the type
instance Functor Tokenizer where
  fmap f (Tokenizer t) = Tokenizer $ fmap (first f) . t
    where
      first f (a, b) = (f a, b)

-- | Squash two tokenizers together
instance Applicative Tokenizer where
  pure a = Tokenizer (\input -> Right (a, input))
  Tokenizer tF <*> Tokenizer tA =
    Tokenizer $ \input -> do
      (f, rest) <- tF input
      (a, s) <- tA rest
      return (f a, s)

-- | Chooses between two tokenizers, if both are correct, chooses the longest one
instance Alternative Tokenizer where
  empty = Tokenizer $ Left . unexpected
  Tokenizer tA <|> Tokenizer tB =
    Tokenizer $ \input -> case (tA input, tB input) of
      (res, Left _) -> res
      (Left _, res) -> res
      -- Longest match rule
      (a@(Right (_, restA)), b@(Right (_, restB))) ->
        if B8.length restA <= B8.length restB then a else b

-- | A tokenizer that matches a single character matching a predicate
satisfies :: (Char -> Bool) -> Tokenizer Char
satisfies p =
  Tokenizer $ \bs -> case B8.uncons bs of
    Just (c, cs)
      | p c -> Right (c, cs)
      | otherwise -> Left (unexpected cs)
    Nothing -> Left (unexpected bs)

-- | A tokenizer that matches a single character (case insensitive)
char :: Char -> Tokenizer Char
char target =
  satisfies
    ( \x -> x == target || x == toUpper target || x == toLower target
    )

-- | A tokenizer that matches a string
string :: Text -> Tokenizer Text
string target = strip (T.pack <$> traverse char (T.unpack target))

stringSatisfies :: (Char -> Bool) -> Tokenizer Text
stringSatisfies p = T.pack <$> some (satisfies p)

-- | Map for attributes : Attribute Name -> Attribute Value
type AttributeMap = Map.Map Text Text

-- | Represents types of Tokens we can tokenize
-- The tokenizer will produce a list of tokens
data Token
  = -- Text Node
    TextToken Text
  | -- <a>
    StartTagToken Text AttributeMap
  | -- </a>
    EndTagToken Text
  | -- <br/>
    SelfClosingTagToken Text AttributeMap
  | -- <!--x-->
    CommentToken Text
  | -- <!DOCTYPE x>
    DoctypeToken Text
  deriving (Eq, Show)

-- ** Tokenizers

-- These represent tokenizers to match each type token and helper Tokenizers

-- | Tokenizer to recognize list of tokens (final tokenizer)
tokens :: Tokenizer [Token]
tokens = some (text <|> startTag <|> endTag <|> selfClosingTag <|> commentTag <|> doctypeTag)

-- | Tokenizer recognzing text node
text :: Tokenizer Token
text =
  many whiteSpace
    *> some (satisfies (/= '<'))
      |> fmap (TextToken . T.strip . T.pack)

-- | Tokenizer recognizing start tag
startTag :: Tokenizer Token
startTag =
  lt
    *> tagName `as` StartTagToken `with` many attribute
    <* gt

-- | Tokenizer recognizign end tag
endTag :: Tokenizer Token
endTag =
  string "</"
    *> tagName
    <* gt
      |> fmap EndTagToken

-- | Tokenizer recognizing self closing tag
selfClosingTag :: Tokenizer Token
selfClosingTag =
  lt
    *> tagName `as` SelfClosingTagToken `with` many attribute
    <* string "/>"

-- | Tokenizer recognizing comment tag
commentTag :: Tokenizer Token
commentTag =
  string "<!--"
    *> nEqString
    <* string "-->"
      |> fmap CommentToken
  where
    nEqString :: Tokenizer Text
    nEqString = Tokenizer $ \bs ->
      Right $ first (decodeUtf8Lenient . B8.strip) $ B8.breakSubstring "-->" bs

-- | Tokenizer recognizing doctype tag
doctypeTag :: Tokenizer Token
doctypeTag =
  string "<!"
    *> string "doctype"
    *> stringSatisfies (/= '>')
    <* gt
      |> fmap (DoctypeToken . T.strip)

-- | Tokenizer recognizing element attributes
attribute :: Tokenizer (Text, Text)
attribute =
  strip $
    liftA2
      (,)
      ( validFirstAttrNameChar `followedByMany` validAttrNameChar
          <* eq
      )
      ( -- double quoted attribute value : class="x"
        ( char '"'
            *> stringSatisfies (/= '"')
            <* char '"'
        )
          <|>
          -- single quoted attribute value : class='x'
          ( char '\''
              *> stringSatisfies (/= '\'')
              <* char '\''
          )
          <|>
          -- unquoted attribute value : class=x
          stringSatisfies (\x -> not (isSpace x) && x /= '>' && x /= '/')
      )
  where
    validFirstAttrNameChar =
      alpha
        <|> char '_'
        <|> char ':'

    validAttrNameChar =
      alphaNum
        <|> char '_'
        <|> char ':'
        <|> char '.'
        <|> char '-'

-- | Tokenizer recognizing tag names
tagName :: Tokenizer Text
tagName =
  strip $ validFirstTagChar `followedByMany` validTagChar
  where
    validFirstTagChar :: Tokenizer Char
    validFirstTagChar = alpha <|> char '_' <|> char ':'

    validTagChar :: Tokenizer Char
    validTagChar =
      alphaNum
        <|> char '-'
        <|> char ':'
        <|> char '.'
        <|> char '_'

-- | Tokenizer recognizing '<'
lt :: Tokenizer Char
lt = strip $ char '<'

-- | Tokenizer recognizing '>'
gt :: Tokenizer Char
gt = strip $ char '>'

-- | Tokenizer recognizing '='
eq :: Tokenizer Char
eq = strip $ char '='

-- | Tokenizer recognizing whitespace
whiteSpace :: Tokenizer Char
whiteSpace = satisfies isSpace

-- | Tokenizer recognizing alphabetic characters
alpha :: Tokenizer Char
alpha = satisfies isAlpha

-- | Tokenizer recognizing alphanumeric characters
alphaNum :: Tokenizer Char
alphaNum = satisfies isAlphaNum

-- *** Tokenizer helper functions

-- | Strips whitespace from before and after a tokenizer
strip :: Tokenizer a -> Tokenizer a
strip t = many whiteSpace *> t <* many whiteSpace

-- | Begins constructing attributed token tokenizer with tag name tokenizer
as :: Tokenizer Text -> (Text -> AttributeMap -> Token) -> Tokenizer (AttributeMap -> Token)
as t1 f = f <$> t1

-- | Constructs attributed token tokenizer from attribute map tokenizer
with :: Tokenizer (AttributeMap -> Token) -> Tokenizer [(Text, Text)] -> Tokenizer Token
with t1 t2 = t1 <*> (Map.fromList <$> t2)

-- | Merges tokenizers of char and many char
followedByMany :: Tokenizer Char -> Tokenizer Char -> Tokenizer Text
followedByMany t1 t2 = liftA2 T.cons t1 (T.pack <$> many t2)

-- | Forward pipe operator
infixr 1 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

-- ** Tokenizer

tokenizer :: ByteString -> Either TokenizerError [Token]
tokenizer input = fst <$> runTokenizer tokens input
