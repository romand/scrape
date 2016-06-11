module Scrape.PageExp.ParserSpec where

import Test

import Scrape.Page.Parser
import Data.Attoparsec.Text (parseOnly)
import Data.Either (isRight)

spec :: Spec
spec = do

  describe "p_Expr" $ do

    let ok s = parseOnly p_Expr s `shouldSatisfy` isRight

    it "parses field name and selector" $
      ok "title = title"

  describe "page" $ do

    let ok s = parseOnly p_PageExp s `shouldSatisfy` isRight

    it "parses normal page" $ do
      pageStr <- readFile "test/data/good.scraper"
      ok pageStr

  describe "loadPages" $ do

    it "loads pages from files" $ do
      let paths = ["test/data/good.scraper", "test/data/bad.scraper"]
      (pages, errors) <- loadPages paths
      (length pages, length errors) `shouldBe` (1, 1)
