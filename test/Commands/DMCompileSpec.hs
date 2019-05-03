module Commands.DMCompileSpec (
    spec
    ) where

import Protolude

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Commands.DMCompile
import Data.List

spec :: Spec
spec = parallel $ do
    describe "Command.DMCompile" $ do
        describe "uniq" $ do
            it "should return a list with <= length" $ property $
                \xs -> length (uniq xs) <= length (xs :: [Int])

            it "should remove duplicates" $ property $
                \xs -> all (\ys -> length ys == 1) $ group (uniq (xs :: [Int]))

            it "should preserve the original ordering" $ do
                uniq [1, 1, 8, 9, 1, 3, 8, 2] `shouldBe` [1, 8, 9, 3, 2]



