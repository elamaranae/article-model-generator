module Main where

import System.IO  
import Control.Monad
import qualified Data.Text.IO as Txt
import Data.Text (unpack)
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B

import Lib (markdownToJSON)

main :: IO ()
main = do
  args <- getArgs
  contents <- Txt.readFile $ head args
  let json = markdownToJSON contents
  B.putStrLn json
