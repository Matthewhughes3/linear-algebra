{-# LANGUAGE OverloadedLists #-}

import Lib
import Test.Hspec

main :: IO ()
main = hspec $
  describe "matrix multiplication" $ do
    it "multiplies non-square matricies" $ ([[1, 4], [2, 5], [3, 6]] `matrixMultiply` [[7, 9, 11], [8, 10, 12]]) `shouldBe` [[58, 139], [64, 154]]
    it "multiplies square matricies" $ ([[0, 1], [2, 0]] `matrixMultiply` [[1, 1], [-2, 0]]) `shouldBe` [[2, 1], [0, -2]]