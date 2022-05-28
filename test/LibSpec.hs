{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LibSpec where

import Data.Aeson (Value, encode)
import Data.Aeson.QQ.Simple (aesonQQ)
import Test.Hspec
import Text.RawString.QQ(r)
import Lib

spec :: Spec
spec = do
  describe "convert markdown to JSON" $ do
    it "plain text" $ do
      markdownToJSON "simple" `shouldBe`
        [r|{"data":[{"data":[{"markup":[],"content":"simple"}],"type":"paragraph"}],"type":"document"}|]

    it "strong" $ do
      markdownToJSON "**simple**" `shouldBe`
        [r|{"data":[{"data":[{"markup":["strong"],"content":"simple"}],"type":"paragraph"}],"type":"document"}|]

    it "strong and emphasis" $ do
      markdownToJSON "**_simple_**" `shouldBe`
        [r|{"data":[{"data":[{"markup":["strong","emphasis"],"content":"simple"}],"type":"paragraph"}],"type":"document"}|]

    it "nested styles" $ do
      markdownToJSON "*start **simple** end*" `shouldBe`
        [r|{"data":[{"data":[{"markup":["emphasis"],"content":"start "},{"markup":["emphasis","strong"],"content":"simple"},{"markup":["emphasis"],"content":" end"}],"type":"paragraph"}],"type":"document"}|]

    it "soft breaks" $ do
      markdownToJSON "simple\nand a break" `shouldBe`
        [r|{"data":[{"data":[{"markup":[],"content":"simple"},{"markup":[],"content":" "},{"markup":[],"content":"and a break"}],"type":"paragraph"}],"type":"document"}|]

    it "soft breaks at end" $ do
      markdownToJSON "simple\nand a break\n" `shouldBe`
        [r|{"data":[{"data":[{"markup":[],"content":"simple"},{"markup":[],"content":" "},{"markup":[],"content":"and a break"}],"type":"paragraph"}],"type":"document"}|]

    it "headers" $ do
      markdownToJSON "# main\n## sub" `shouldBe` [r|{"data":[{"data":[{"markup":[],"content":"main"}],"type":"header","level":1},{"data":[{"markup":[],"content":"sub"}],"type":"header","level":2}],"type":"document"}|]

    it "nested markdown across multiple paragraphs" $ do
      markdownToJSON "**history *is* nice**\n\n*however **geography** rocks*" `shouldBe`
        [r|{"data":[{"data":[{"markup":["strong"],"content":"history "},{"markup":["strong","emphasis"],"content":"is"},{"markup":["strong"],"content":" nice"}],"type":"paragraph"},{"data":[{"markup":["emphasis"],"content":"however "},{"markup":["emphasis","strong"],"content":"geography"},{"markup":["emphasis"],"content":" rocks"}],"type":"paragraph"}],"type":"document"}|]

    it "code blocks" $ do
      markdownToJSON [r|
```
var a = 5
return
```
      |] `shouldBe` encode [aesonQQ|
{
  "data": [
    {
      "data": [ { "markup": [], "content": "var a = 5\nreturn\n" } ],
      "type": "code_block"
    }
  ],
  "type": "document"
}
      |]
