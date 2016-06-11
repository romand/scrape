{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test (module Test.Hspec, module Test) where

import GHC.Stack (errorWithStackTrace)
import ClassyPrelude as Test hiding (undefined, error)

import Test.Hspec

error :: String -> a
error = errorWithStackTrace

fromRight :: (Show a) => Either a b -> b
fromRight = either (error . show) id

undefined :: a
undefined = error "should not be evaluated"

withHtml :: String -> (String -> IO ()) -> IO ()
withHtml path = bracket (readFile path) (const $ return ())
