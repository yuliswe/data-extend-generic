{-# LANGUAGE DeriveGeneric #-}

import Test.Hspec
import Data.Extend
import GHC.Generics


data Test = Test | TestA {
      f1 :: Int,
      f2 :: Maybe Int,
      f3 :: Maybe Int
   } | TestB {
      g1 :: Int,
      g2 :: Maybe Int,
      g3 :: Maybe Int
   } deriving (Show, Generic, Eq)

test1A = TestA 1 (Just 1) Nothing
test2A = TestA 1 Nothing (Just 2)

test1B = TestB 1 (Just 1) Nothing
test2B = TestB 1 Nothing (Just 2)



instance Extend Test

main :: IO ()
main = hspec $
   describe "Data.Extend" $ do
      specify "Int" $
         (1 `extend` 2) `shouldBe` (2 :: Int)
      specify "String" $
         ("a" `extend` "b") `shouldBe` "b"
      specify "data 0" $
         (Test `extend` Test) `shouldBe` Test
      specify "data 1" $
         (test1A `extend` test2A) `shouldBe` TestA 1 (Just 1) (Just 2)
      specify "data 2" $
         (test1B `extend` test2B) `shouldBe` TestB 1 (Just 1) (Just 2)
      specify "data 3" $
         (test1A `extend` test2B) `shouldBe` test2B
      specify "data 4" $
         (test2B `extend` test1A) `shouldBe` test1A
      specify "data 5" $
         (test1B `extend` test2A) `shouldBe` test2A
      specify "data 6" $
         (test2A `extend` test1B) `shouldBe` test1B


