{-# LANGUAGE Safe #-}
----------------------------------------------------------------------------------
-- |
-- Module      : Data.Extend
-- Copyright   : (c) Yu Li, 2015
-- License     : BSD
-- Maintainer  : ylilarry@gmail.com
-- Stability   : experimental
-- Portability : GHC extensions
--
-- This package allows you to extend a Haskell data like how you do in OOP.
--
-- Here is an example use for testing:
--
-- >
-- > data Test = Test | TestA {
-- >       f1 :: Int,
-- >       f2 :: Maybe Int,
-- >       f3 :: Maybe Int
-- >    } deriving (Show, Generic, Eq)
-- >
-- > test1A = TestA 1 (Just 1) (Just 3)
-- > test2A = TestA 0 Nothing (Just 2)
-- >
-- > main :: IO ()
-- > main = hspec $
-- >    describe "Data.Extend" $ do
-- >       specify "Int" $
-- >          (2 `extend` 1) `shouldBe` (2 :: Int)
-- >       specify "String" $
-- >          ("b" `extend` "a") `shouldBe` "b"
-- >       specify "data 0" $
-- >          (Test `extend` Test) `shouldBe` Test
-- >       specify "data 1" $
-- >          (test2A `extend` test1A) `shouldBe` TestA 0 (Just 1) (Just 2)
-- >
--
----------------------------------------------------------------------------------

module Data.Extend (
      Extend(..)
   ) where

import Data.Extend.Internal
