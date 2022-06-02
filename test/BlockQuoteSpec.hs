{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BlockQuoteSpec where

import Data.Aeson (Value, encode)
import Data.Aeson.QQ.Simple (aesonQQ)
import Test.Hspec
import Text.RawString.QQ(r)
import Lib
import qualified CMark as C

spec :: Spec
spec = do
  describe "convert markdown to JSON" $ do
    it "block quote" $ do
      markdownToJSON [r|
> This is a simple block quote
  spans to next line
      |] `shouldBe` encode [aesonQQ|
{
  "data": [
    {
      "data": [
        {
          "data": [
            { "markup": [], "content": "This is a simple block quote" },
            { "markup": [], "content": " " },
            { "markup": [], "content": "spans to next line" }
          ],
          "type": "paragraph"
        }
      ],
      "type": "block_quote"
    }
  ],
  "type": "document"
}
      |]

    it "block quote 2" $ do
      markdownToJSON [r|
> This is a simple block quote
> spans to next line
      |] `shouldBe` encode [aesonQQ|
{
  "data": [
    {
      "data": [
        {
          "data": [
            { "markup": [], "content": "This is a simple block quote" },
            { "markup": [], "content": " " },
            { "markup": [], "content": "spans to next line" }
          ],
          "type": "paragraph"
        }
      ],
      "type": "block_quote"
    }
  ],
  "type": "document"
}
      |]
