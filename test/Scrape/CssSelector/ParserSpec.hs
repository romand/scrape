module Scrape.CssSelector.ParserSpec where

import Test

import Scrape.CssSelector.Types
import Scrape.CssSelector.Parser
import Data.Attoparsec.Text

spec :: Spec
spec = do

  describe "p_RelPrev" $ do

    let p s rp = parseOnly p_RelPrev s `shouldBe` (Right rp)

    it "parses ' '" $ " " `p` Descendant
    it "parses >" $ ">" `p` Child
    it "parses +" $ " +" `p` Next
    it "parses ~" $ "~" `p` Sibling

  describe "p_AttrVal" $ do

    let s ~> av = parseOnly p_AttrVal s `shouldBe` (Right av)

    it "parses no relation" $ " " ~> Nothing

    it "parses ="  $ "=a" ~> Just (Equal, "a")
    it "parses ^=" $ "^= a" ~> Just (PrefixMatch, "a")
    it "parses $=" $ "$= a" ~> Just (SuffixMatch, "a")
    it "parses ~=" $ "~= a" ~> Just (Includes, "a")
    it "parses |=" $ "|= a" ~> Just (DashMatch, "a")
    it "parses *=" $ "*= a" ~> Just (SubstringMatch, "a")

    it "parses ident value" $ "=-ide_nt_-" ~> Just (Equal, "-ide_nt_-")
    it "parses string value" $ "= ' =\"\\' ' " ~> Just (Equal, " =\"' ")

  describe "p_TagAttr" $ do

    let s ~> attr = parseOnly p_TagAttr s `shouldBe` (Right attr)

    it "parses attr presence" $
      "[ disabled]" ~> TagAttr "disabled" Nothing

    it "parses attr with relation" $
      "[class ~= hidden ]" ~> TagAttr "class" (Just (Includes, "hidden"))

    it "parses attr with funky value" $
      "[name *= ']\\]' ]" ~> TagAttr "name" (Just (SubstringMatch, "]]"))

  describe "p_TagAttrs" $ do

    let s ~> attrs = parseOnly p_TagAttrs s `shouldBe` (Right attrs)

    it "parses many attrs" $
      "[disabled][ name *= ']' ][ class ~= hidden ]" ~>
      [ TagAttr "disabled" Nothing
      , TagAttr "name" (Just (SubstringMatch, "]"))
      , TagAttr "class" (Just (Includes, "hidden")) ]

  describe "p_CssSelector" $ do

    let s ~> cs = parseOnly p_CssSelector s `shouldBe` (Right cs)

    it "parses *" $ "*[name]" ~>
      CssSelector Descendant Nothing [TagAttr "name" Nothing]

    it "parses implicit *" $ "[name]" ~>
      CssSelector Descendant Nothing [TagAttr "name" Nothing]

    it "parses element selector" $ "body" ~>
      CssSelector Descendant (Just "body") []

    it "parses class selector" $ ".b" ~>
      CssSelector Descendant Nothing [TagAttr "class" (Just (Includes, "b"))]

    it "parses id selector" $ "#id" ~>
      CssSelector Descendant Nothing [TagAttr "id" (Just (Equal, "id"))]

  describe "parseCssSel" $ do

    let s ~> css = parseCssSel s `shouldBe` (Right css)

    it "parses sequence of selectors" $ "[name] [href]>*.cls~div#id" ~>
      [ CssSelector Descendant Nothing [TagAttr "name" Nothing]
      , CssSelector Descendant Nothing [TagAttr "href" Nothing]
      , CssSelector Child Nothing [TagAttr "class" (Just (Includes, "cls"))]
      , CssSelector Sibling (Just "div") [TagAttr "id" (Just (Equal, "id"))] ]
