{-# LANGUAGE FlexibleContexts #-}

module Import (module Import) where

import GHC.Stack (errorWithStackTrace)
import ClassyPrelude as Import hiding (error)

error :: String -> a
error = errorWithStackTrace

fromRight :: (Show a) => Either a b -> b
fromRight = either (error . show) id
