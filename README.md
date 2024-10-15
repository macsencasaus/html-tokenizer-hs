# HTML Tokenizer

HTML tokenizer library for Haskell.
Tokenizes HTML elements from bytestring.

The single [Tokenizer.hs](./Tokenizer.hs) file exposes types

```hs
-- | Tokenizer errors, represents errors that can occur
data TokenizerError
  = -- Encountered a character it wasn't expecting to see
    Unexpected Char
  | -- Encountered EOF before it was expected
    UnexpectedEOF
  deriving (Eq, Show)
```

and

```hs
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
```

where

```hs
-- | Map for attributes : Attribute Name -> Attribute Value
type AttributeMap = Map.Map Text Text
```

as well as one function

```hs
tokenizer :: ByteString -> Either TokenizerError [Token]
```

used to tokenize bytestring of HTML elements.

See [Example.hs](./Example.hs) for a small example of a program used to scrape the title of a website using the tokenizer.

## Dependencies

For the tokenizer:
```
build-depends:
  base >= 4.14 && < 5,           
  bytestring >= 0.10.12.0,      
  text >= 1.2.4.0,             
  containers >= 0.6.2.0          
```

For the example:
```
  http-conduit >= 2.3.0          
```

For the tests:
```
  hspec >= 2.9.0                 
```

## Resources

- [(Haskell in Haskell) 2. Lexing](https://cronokirby.com/posts/2020/12/haskell-in-haskell-2/)
