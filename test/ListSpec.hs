{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ListSpec where

import Data.Aeson (Value, encode)
import Data.Aeson.QQ.Simple (aesonQQ)
import Test.Hspec
import Text.RawString.QQ(r)
import Lib
import qualified CMark as C

spec :: Spec
spec = do
  describe "convert markdown to JSON" $ do
    it "ordered list items" $ do
      markdownToJSON [r|
1. first
2. second
      |] `shouldBe` encode [aesonQQ|
{
  "data": [
    {
      "data": [
        [{
          "data": [ { "markup": [], "content": "first" } ],
          "type": "paragraph"
        }],
        [{
          "data": [ { "markup": [], "content": "second" } ],
          "type": "paragraph"
        }]
      ],
      "type": "list",
      "list_type": "ordered_list"
    }
  ],
  "type": "document"
}
      |]

    it "ordered list with soft breaks and italics" $ do
      markdownToJSON [r|
1. first *step
   is* something
2. second
      |] `shouldBe` encode [aesonQQ|
{
  "data": [
    {
      "data": [
        [{
          "data": [
            { "markup": [], "content": "first " },
            { "markup": [ "emphasis" ], "content": "step" },
            { "markup": [ "emphasis" ], "content": " " },
            { "markup": [ "emphasis" ], "content": "is" },
            { "markup": [], "content": " something" }
          ],
          "type": "paragraph"
        }],
        [{
          "data": [ { "markup": [], "content": "second" } ],
          "type": "paragraph"
        }]
      ],
      "type": "list",
      "list_type": "ordered_list"
    }
  ],
  "type": "document"
}
      |]

    it "nested list items" $ do
      markdownToJSON [r|
1. This is first.

2. This is second.

    Nested text.

    ```javascript
    nested code
    ```

3. This is third.

    - Nested One
    - Nested Two
      |] `shouldBe` encode [aesonQQ|
{
  "data": [
    {
      "data": [
        [
          {
            "data": [ { "markup": [], "content": "This is first." } ],
            "type": "paragraph"
          }
        ],
        [
          {
            "data": [ { "markup": [], "content": "This is second." } ],
            "type": "paragraph"
          },
          {
            "data": [ { "markup": [], "content": "Nested text." } ],
            "type": "paragraph"
          },
          {
            "data": [ { "markup": [], "content": "nested code\n" } ],
            "type": "code_block"
          }
        ],
        [
          {
            "data": [ { "markup": [], "content": "This is third." } ],
            "type": "paragraph"
          },
          {
            "data": [
              [{
                "data": [ { "markup": [], "content": "Nested One" } ],
                "type": "paragraph"
              }],
              [{
                "data": [ { "markup": [], "content": "Nested Two" } ],
                "type": "paragraph"
              }]
            ],
            "type": "list",
            "list_type": "ordered_list"
          }
        ]
      ],
      "type": "list",
      "list_type": "ordered_list"
    }
  ],
  "type": "document"
}
      |]
