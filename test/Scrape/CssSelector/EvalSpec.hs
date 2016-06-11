module Scrape.CssSelector.EvalSpec where

import Test hiding (readFile)

import Scrape.CssSelector.Eval
import Scrape.CssSelector.Parser (parseCssSel)
import System.IO.Unsafe (unsafePerformIO)
import Text.HTML.DOM (readFile)
import Text.XML.Cursor

spec :: Spec
spec = do

  describe "eval" $ do

    let root = fromDocument $ unsafePerformIO $ readFile "test/data/sample.html"

    let s ~> res = let cs = eval (fromRight $ parseCssSel s) root
                   in map (\c -> concat (c $/ content)) cs `shouldBe` res

    it "selects by element name" $
      "title" ~> [" Motorola Droid 4 "]
