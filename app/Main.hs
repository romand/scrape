module Main where

import Scrape.Page.Parser (loadPages)
import Scrape.Eval (evalPages)

import Control.Monad (when)
import System.Environment (getArgs)
import Data.Text (intercalate, unpack)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) $
    error "Usage: scrape http://mysite.com/page1.html page1.html page1.scraper ..."

  let url:htmlPath:pagesPaths = args

  (errors, pages) <- loadPages pagesPaths

  when (not $ null errors) $
    error $ unpack $ intercalate "\n" ("failed to parse scrapers:":errors)

  result <- evalPages url htmlPath pages

  case result of
    Left e -> putStrLn $ "failed to scrape: " ++ show e
    Right r -> L.putStrLn $ encode r
