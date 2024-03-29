{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- OurDataSpecs
-}

module OurDataSpecs (
    ourDataSpecs
) where

import Test.Hspec
import Control.Exception (evaluate)
import OurData (
        Color(..),
        Pixel(..)
    )

ourDataSpecs :: Spec
ourDataSpecs = describe "OurData Functions :" $ do
    showColorSpec
    showPixelSpec
    numColorSpec
    dataColorSpec
    dataPixelSpec

showColorSpec :: Spec
showColorSpec = describe "Function showColor :" $ do
    it "should return the string representation of a color" $ do
        show (Color 0 0 0) `shouldBe` "(0,0,0)"
        show (Color 255 255 255) `shouldBe` "(255,255,255)"
        show (Color 255 0 0) `shouldBe` "(255,0,0)"
        show (Color 0 255 0) `shouldBe` "(0,255,0)"
        show (Color 0 0 255) `shouldBe` "(0,0,255)"

showPixelSpec :: Spec
showPixelSpec = describe "Function showPixel :" $ do
    it "should return the string representation of a pixel" $ do
        show (Pixel 0 0 (Color 0 0 0)) `shouldBe` "(0,0) (0,0,0)"
        show (Pixel 0 0 (Color 255 255 255)) `shouldBe` "(0,0) (255,255,255)"
        show (Pixel 0 0 (Color 255 0 0)) `shouldBe` "(0,0) (255,0,0)"
        show (Pixel 0 0 (Color 0 255 0)) `shouldBe` "(0,0) (0,255,0)"
        show (Pixel 0 0 (Color 0 0 255)) `shouldBe` "(0,0) (0,0,255)"

numColorSpec :: Spec
numColorSpec = describe "Function numColor :" $ do
    it "should return the addition of two colors" $ do
        ((Color 0 0 0) + (Color 0 0 0)) `shouldBe` Color 0 0 0
        ((Color 10 10 10) + (Color 10 10 10)) `shouldBe` Color 20 20 20
    it "should raise an error for the multiplication of two colors" $
        evaluate ((Color 0 0 0) * (Color 0 0 0)) `shouldThrow` anyException
    it "should raise an error for the abs of a color" $
        evaluate (abs (Color 0 0 0)) `shouldThrow` anyException
    it "should raise an error for the signum of a color" $
        evaluate (signum (Color 0 0 0)) `shouldThrow` anyException
    it "should raise an error for the negate of a color" $
        evaluate (negate (Color 0 0 0)) `shouldThrow` anyException
    it "should raise an error for the fromInteger of a color" $
        evaluate (fromInteger 0 :: Color) `shouldThrow` anyException

dataColorSpec :: Spec
dataColorSpec = describe "Color accessors" $ do
        it "should return correct red value" $
            let c = Color 255 0 0
            in red c `shouldBe` 255
        it "should return correct green value" $
            let c = Color 0 255 0
            in green c `shouldBe` 255
        it "should return correct blue value" $
            let c = Color 0 0 255
            in blue c `shouldBe` 255

dataPixelSpec :: Spec
dataPixelSpec = describe "Pixel accessors" $ do
        it "should return correct x value" $
            let p = Pixel 0 0 (Color 0 0 0)
            in x p `shouldBe` 0
        it "should return correct y value" $
            let p = Pixel 0 0 (Color 0 0 0)
            in y p `shouldBe` 0
        it "should return correct color value" $
            let p = Pixel 0 0 (Color 0 0 0)
            in color p `shouldBe` Color 0 0 0
