{-# LANGUAGE OverloadedStrings #-}

module Lib ( markdownToJSON) where

import Data.Aeson (ToJSON(..), Value(..), object, (.=), encode)
import qualified CMark as C
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)

newtype Markdown = Markdown [Entity]

data Entity = Paragraph [Content]
            | Header [Content] Int
            | CodeBlock [Content]
            | List [Entity]

data Markup = Strong
            | Emphasis
            | Code
            deriving (Eq, Show)

data Content = Content Text [Markup]

markdownToJSON :: Text -> ByteString
markdownToJSON = encode . nodeToInternalType . C.commonmarkToNode []

nodeToInternalType :: C.Node -> Markdown
nodeToInternalType (C.Node _ _ children) = Markdown $ map processEntity children

processEntity :: C.Node -> Entity
processEntity (C.Node _ C.PARAGRAPH children) = Paragraph (children >>= processContents)
processEntity (C.Node _ (C.HEADING level) children) = Header (children >>= processContents) level
processEntity (C.Node _ (C.CODE_BLOCK _ text) children) = CodeBlock [Content text []]
processEntity (C.Node _ (C.LIST attributes) children) = List (map processListItem children)

processListItem :: C.Node -> Entity
processListItem (C.Node _ C.ITEM (child:_)) = processEntity child

processContents :: C.Node -> [Content]
processContents (C.Node _ (C.TEXT text) _) = [Content text []]
processContents (C.Node _ (C.CODE text) _) = [Content text [Code]]
processContents (C.Node _ C.SOFTBREAK _) = [Content " " []]
processContents (C.Node _ C.STRONG children) = map (addMarkup Strong) $ children >>= processContents
processContents (C.Node _ C.EMPH children) = map (addMarkup Emphasis) $ children >>= processContents
processContents a = []

addMarkup :: Markup -> Content -> Content
addMarkup markup (Content t markups) = Content t (markup:markups)

instance ToJSON Markdown where
  toJSON (Markdown entities) = object
    [
      "data" .= map toJSON entities,
      "type" .= ("document" :: String)
    ]

instance ToJSON Entity where
  toJSON (Paragraph contents) = object
    [
      "data" .= map toJSON contents,
      "type" .= ("paragraph" :: String)
    ]
  toJSON (Header contents level) = object
    [
      "data" .= map toJSON contents,
      "type" .= ("header" :: String),
      "level" .= level
    ]
  toJSON (CodeBlock contents) = object
    [
      "data" .= map toJSON contents,
      "type" .= ("code_block" :: String)
    ]
  toJSON (List entities) = object
    [
      "data" .= map toJSON entities,
      "type" .= ("list" :: String),
      "list_type" .= ("ordered_list" :: String)
    ]

instance ToJSON Content where
  toJSON (Content text markups) = object
    [
      "content" .= text,
      "markup"  .= map (toLowerString.show) markups
    ]

toLowerString :: String -> String
toLowerString = map toLower
