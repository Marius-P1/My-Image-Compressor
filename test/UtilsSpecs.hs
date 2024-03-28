{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- UtilsSpecs
-}

module UtilsSpecs (
    utilsSpecs
) where

import Test.Hspec
import System.Random (mkStdGen)
import OurData (
        Color(..),
        Pixel(..)
    )
import Utils (
        getMaxColor,
        getFailingColor,
        getRandomColor
    )

utilsSpecs :: Spec
utilsSpecs = describe "Utils Functions :" $ do
    getMaxColorSpec
    getFailingColorSpec
    getRandomColorSpec

getMaxColorSpec :: Spec
getMaxColorSpec = describe "Function getMaxColor :" $ do
    it "should return the maximum color" $
        getMaxColor `shouldBe` Color maxBound maxBound maxBound

getFailingColorSpec :: Spec
getFailingColorSpec = describe "Function getFailingColor :" $ do
    it "should return a empty list" $
        getFailingColor 0 `shouldBe` []
    it "should return a list of 1 failing color" $
        getFailingColor 1 `shouldBe` [Color maxBound maxBound maxBound]
    it "should return a list of 2 failing colors" $
        getFailingColor 2 `shouldBe` [Color maxBound maxBound maxBound, Color maxBound maxBound maxBound]
    it "should return a list of 5 failing colors" $
        getFailingColor 5 `shouldBe` [Color maxBound maxBound maxBound, Color maxBound maxBound maxBound, Color maxBound maxBound maxBound, Color maxBound maxBound maxBound, Color maxBound maxBound maxBound]

getRandomColorSpec :: Spec
getRandomColorSpec = describe "Function getRandomColor :" $ do
    it "should return an empty list" $
        getRandomColor (mkStdGen 0) [] 0 `shouldBe` []
    it "should return a list of 1 random color" $
        getRandomColor (mkStdGen 0) [Pixel 0 0 (Color 0 0 0)] 1 `shouldBe` [Color 0 0 0]
    it "should return a list of 2 random colors" $
        getRandomColor (mkStdGen 0) [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1)] 2 `shouldBe` [Color 0 0 0, Color 1 1 1]
    it "should return a list of 5 random colors" $
        getRandomColor (mkStdGen 0) [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2), Pixel 3 3 (Color 3 3 3), Pixel 4 4 (Color 4 4 4)] 5 `shouldBe` [Color 0 0 0, Color 1 1 1, Color 2 2 2, Color 4 4 4, Color 3 3 3]
