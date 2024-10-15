{-# LANGUAGE OverloadedStrings #-}

import Data.Map qualified as Map
import Test.Hspec
import Tokenizer

testStartTag :: Spec
testStartTag = describe "start tag tokenizing" $ do
  it "no attribute" $
    tokenizer "<html>" `shouldBe` Right [StartTagToken "html" Map.empty]

  it "no attribute + whitespace" $
    tokenizer " < \nhtml\r\t > " `shouldBe` Right [StartTagToken "html" Map.empty]

  it "one attribute" $
    tokenizer
      "<p class=\"class1\"> \
      \<div class='class2'> \
      \<h1 class=class3>"
      `shouldBe` Right
        [ StartTagToken "p" (Map.fromList [("class", "class1")]),
          StartTagToken "div" (Map.fromList [("class", "class2")]),
          StartTagToken "h1" (Map.fromList [("class", "class3")])
        ]

  it "several attributes" $
    tokenizer
      "<div class=\"c1 c2 c3\" id=\"id123\"> \
      \< div attr=\"content='hello'\" attr1 = attr1 > \
      \<abc123 a='a1' b='b1' c='c1' d='d1'>"
      `shouldBe` Right
        [ StartTagToken "div" (Map.fromList [("class", "c1 c2 c3"), ("id", "id123")]),
          StartTagToken "div" (Map.fromList [("attr", "content='hello'"), ("attr1", "attr1")]),
          StartTagToken "abc123" (Map.fromList [("a", "a1"), ("b", "b1"), ("c", "c1"), ("d", "d1")])
        ]

testEndTag :: Spec
testEndTag = describe "end tag tokenizing" $ do
  it "no whitespace" $ do
    tokenizer
      "</html>\
      \</p>\
      \</abc123>"
      `shouldBe` Right
        [ EndTagToken "html",
          EndTagToken "p",
          EndTagToken "abc123"
        ]

  it "whitespace" $ do
    tokenizer
      " \t</ html> \
      \</ p >\r\n\
      \     </    abc123 >   "
      `shouldBe` Right
        [ EndTagToken "html",
          EndTagToken "p",
          EndTagToken "abc123"
        ]

testSelfClosingTag :: Spec
testSelfClosingTag = describe "self closing tag tokenizing" $ do
  it "no attribute" $ do
    tokenizer
      "<br/>"
      `shouldBe` Right
        [ SelfClosingTagToken "br" Map.empty
        ]

  it "no attribute + whitespace" $ do
    tokenizer
      "< \t br \r\n />\r\n"
      `shouldBe` Right
        [ SelfClosingTagToken "br" Map.empty
        ]

  it "several attributes" $ do
    tokenizer
      "<img src=\"./cool_image.jpg\"/> \
      \< a class='cool_class' href=\"http://www.coolwebsite.com\"/>"
      `shouldBe` Right
        [ SelfClosingTagToken "img" (Map.fromList [("src", "./cool_image.jpg")]),
          SelfClosingTagToken "a" (Map.fromList [("class", "cool_class"), ("href", "http://www.coolwebsite.com")])
        ]

testText :: Spec
testText = describe "text node tokenzing" $ do
  it "plain text" $
    do
      tokenizer "this is just some text idk"
      `shouldBe` Right [TextToken "this is just some text idk"]

testDoctypeTag :: Spec
testDoctypeTag = describe "doctype tag tokenizing" $ do
  it "doctypes" $
    do
      tokenizer
        "<!doctype html>\
        \<! DOCTYPE html>\
        \<!  DoCtYpE   html > "
        `shouldBe` Right [DoctypeToken "html", DoctypeToken "html", DoctypeToken "html"]

testCommentTag :: Spec
testCommentTag = describe "comment tag tokenizing" $ do
  it "comments" $
    do
      tokenizer
        "<!--this is a comment-->\
        \<!-- another -<> comment! -->"
        `shouldBe` Right
          [ CommentToken "this is a comment",
            CommentToken "another -<> comment!"
          ]

main :: IO ()
main = hspec $ do
  testStartTag
  testEndTag
  testSelfClosingTag
  testText
  testDoctypeTag
  testCommentTag
