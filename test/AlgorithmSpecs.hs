{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- AlgorithmSpecs
-}

module AlgorithmSpecs (
    algorithmSpecs
) where

import Test.Hspec
import OurData (Color(..), Pixel(..))
import Algorithm (
        computeDistance,
        computeDistancePixelToColor,
        computeDistanceColorToColor,
        sumPixels,
        computeCentroidsFromMeans,
        getBiggestDistance,
        getClosestColor,
        filterPixels,
        getOnlyNearestPixels,
        getNewCentroids,
        applyKMeans,
        getFinalClusters
    )

algorithmSpecs :: Spec
algorithmSpecs = describe "Algorithms functions :" $ do
    computeDistanceSpec
    computeDistancePixelToColorSpec
    computeDistanceColorToColorSpec
    sumPixelsSpec
    computeCentroidsFromMeansSpec
    getBiggestDistanceSpec
    getClosestColorSpec
    filterPixelsSpec
    getOnlyNearestPixelsSpec
    getNewCentroidsSpec
    applyKMeansSpec
    getFinalClustersSpec

computeDistanceSpec :: Spec
computeDistanceSpec = describe "Function computeDistance :" $ do
    it "Should return the distance between point : (0, 0, 0) and (0, 0, 0)" $
        computeDistance (0, 0, 0) (0, 0, 0) `shouldBe` 0.0
    it "Should return the distance between point : (0, 0, 0) and (1, 1, 1)" $
        computeDistance (0, 0, 0) (1, 1, 1) `shouldBe` sqrt 3.0
    it "Should return the distance between point : (0, 0, 0) and (1, 0, 0)" $
        computeDistance (0, 0, 0) (1, 0, 0) `shouldBe` 1.0
    it "Should return the distance between point : (10, 10, 10) and (10, 10, 10) " $
        computeDistance (10, 10, 10) (10, 10, 10) `shouldBe` 0.0
    it "Should return the distance between point : (10, 10, 10) and (0, 0, 0) " $
        computeDistance (10, 10, 10) (0, 0, 0) `shouldBe` sqrt 300.0

computeDistancePixelToColorSpec :: Spec
computeDistancePixelToColorSpec = describe "Function computeDistancePixelToColor :" $ do
    it "Should return the distance between pixel : (0 0 (0, 0, 0)) and color : (0, 0, 0)" $
        computeDistancePixelToColor (Pixel 0 0 (Color 0 0 0)) (Color 0 0 0) `shouldBe` 0.0
    it "Should return the distance between pixel : (0 0 (0, 0, 0)) and color : (1, 1, 1)" $
        computeDistancePixelToColor (Pixel 0 0 (Color 0 0 0)) (Color 1 1 1) `shouldBe` sqrt 3.0
    it "Should return the distance between pixel : (0 0 (0, 0, 0)) and color : (1, 0, 0)" $
        computeDistancePixelToColor (Pixel 0 0 (Color 0 0 0)) (Color 1 0 0) `shouldBe` 1.0
    it "Should return the distance between pixel : (10 10 (10, 10, 10)) and color : (10, 10, 10) " $
        computeDistancePixelToColor (Pixel 10 10 (Color 10 10 10)) (Color 10 10 10) `shouldBe` 0.0
    it "Should return the distance between pixel : (10 10 (10, 10, 10)) and color : (0, 0, 0) " $
        computeDistancePixelToCo5 5 5lor (Pixel 10 10 (Color 10 10 10)) (Color 0 0 0) `shouldBe` sqrt 300.0

computeDistanceColorToColorSpec :: Spec
computeDistanceColorToColorSpec = describe "Function computeDistanceColorToColor :" $ do
    it "Should return the distance between color : (0, 0, 0) and (0, 0, 0)" $
        computeDistanceColorToColor (Color 0 0 0) (Color 0 0 0) `shouldBe` 0.0
    it "Should return the distance between color : (0, 0, 0) and (1, 1, 1)" $
        computeDistanceColorToColor (Color 0 0 0) (Color 1 1 1) `shouldBe` sqrt 3.0
    it "Should return the distance between color : (0, 0, 0) and (1, 0, 0)" $
        computeDistanceColorToColor (Color 0 0 0) (Color 1 0 0) `shouldBe` 1.0
    it "Should return the distance between color : (10, 10, 10) and (10, 10, 10) " $
        computeDistanceColorToColor (Color 10 10 10) (Color 10 10 10) `shouldBe` 0.0
    it "Should return the distance between color : (10, 10, 10) and (0, 0, 0) " $
        computeDistanceColorToColor (Color 10 10 10) (Color 0 0 0) `shouldBe` sqrt 300.0

sumPixelsSpec :: Spec
sumPixelsSpec = describe "Function sumPixels :" $ do
    it "Should return the sum of pixels : [(0, 0, 0), (0, 0, 0), (0, 0, 0)]" $
        sumPixels [Pixel 0 0 (Color 0 0 0), Pixel 0 0 (Color 0 0 0), Pixel 0 0 (Color 0 0 0)] `shouldBe` Color 0 0 0
    it "Should return the sum of pixels : [(1, 1, 1), (1, 1, 1), (1, 1, 1)]" $
        sumPixels [Pixel 1 1 (Color 1 1 1), Pixel 1 1 (Color 1 1 1), Pixel 1 1 (Color 1 1 1)] `shouldBe` Color 3 3 3
    it "Should return the sum of pixels : [(1, 1, 1), (2, 2, 2), (3, 3, 3)]" $
        sumPixels [Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2), Pixel 3 3 (Color 3 3 3)] `shouldBe` Color 6 6 6
    it "Should return the sum of pixels : [(10, 10, 10), (10, 10, 10), (10, 10, 10)]" $
        sumPixels [Pixel 10 10 (Color 10 10 10), Pixel 10 10 (Color 10 10 10), Pixel 10 10 (Color 10 10 10)] `shouldBe` Color 30 30 30
    it "Should return the sum of pixels : []" $
        sumPixels [] `shouldBe` Color 0 0 0
    it "Should return the sum of pixels : [(1, 6, 5)]" $
        sumPixels [Pixel 1 6 (Color 1 6 5)] `shouldBe` Color 1 6 5

computeCentroidsFromMeansSpec :: Spec
computeCentroidsFromMeansSpec = describe "Function computeCentroidsFromMeans :" $ do
    it "Should return the mean of pixels : [(0, 0, 0), (0, 0, 0), (0, 0, 0)]" $
        computeCentroidsFromMeans [Pixel 0 0 (Color 0 0 0), Pixel 0 0 (Color 0 0 0), Pixel 0 0 (Color 0 0 0)] `shouldBe` Color 0 0 0
    it "Should return the mean of pixels : [(1, 1, 1), (1, 1, 1), (1, 1, 1)]" $
        computeCentroidsFromMeans [Pixel 1 1 (Color 1 1 1), Pixel 1 1 (Color 1 1 1), Pixel 1 1 (Color 1 1 1)] `shouldBe` Color 1 1 1
    it "Should return the mean of pixels : [(1, 1, 1), (2, 2, 2), (3, 3, 3)]" $
        computeCentroidsFromMeans [Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2), Pixel 3 3 (Color 3 3 3)] `shouldBe` Color 2 2 2
    it "Should return the mean of pixels : [(10, 10, 10), (10, 10, 10), (10, 10, 10)]" $
        computeCentroidsFromMeans [Pixel 10 10 (Color 10 10 10), Pixel 10 10 (Color 10 10 10), Pixel 10 10 (Color 10 10 10)] `shouldBe` Color 10 10 10
    it "Should return the mean of pixels : []" $
        computeCentroidsFromMeans [] `shouldBe` Color 0 0 0
    it "Should return the mean of pixels : [(1, 6, 5)]" $
        computeCentroidsFromMeans [Pixel 1 6 (Color 1 6 5)] `shouldBe` Color 1 6 5

getBiggestDistanceSpec :: Spec
getBiggestDistanceSpec = describe "Function getBiggestDistance :" $ do
    it "Should return the biggest distance between colors : [(0, 0, 0), (0, 0, 0), (0, 0, 0)] and [(0, 0, 0), (0, 0, 0), (0, 0, 0)]" $
        getBiggestDistance [Color 0 0 0, Color 0 0 0, Color 0 0 0] [Color 0 0 0, Color 0 0 0, Color 0 0 0] `shouldBe` 0.0
    it "Should return the biggest distance between colors : [(1, 1, 1), (1, 1, 1), (1, 1, 1)] and [(1, 1, 1), (1, 1, 1), (1, 1, 1)]" $
        getBiggestDistance [Color 1 1 1, Color 1 1 1, Color 1 1 1] [Color 1 1 1, Color 1 1 1, Color 1 1 1] `shouldBe` 0.0
    it "Should return the biggest distance between colors : [(1, 1, 1), (2, 2, 2), (3, 3, 3)] and [(1, 1, 1), (2, 2, 2), (3, 3, 3)]" $
        getBiggestDistance [Color 1 1 1, Color 2 2 2, Color 3 3 3] [Color 1 1 1, Color 2 2 2, Color 3 3 3] `shouldBe` 0.0
    it "Should return the biggest distance between colors : [(10, 10, 10), (10, 10, 10), (10, 10, 10)] and [(10, 10, 10), (10, 10, 10), (10, 10, 10)]" $
        getBiggestDistance [Color 10 10 10, Color 10 10 10, Color 10 10 10] [Color 10 10 10, Color 10 10 10, Color 10 10 10] `shouldBe` 0.0
    it "Should return the biggest distance between colors : [] and []" $
        getBiggestDistance [] [] `shouldBe` -1.0
    it "Should return the biggest distance between colors : [(1, 6, 5)] and [(1, 6, 5)]" $
        getBiggestDistance [Color 1 6 5] [Color 1 6 5] `shouldBe` 0.0
    it "Should return the biggest distance between colors : [(1, 6, 5)] and [(10, 25, 36)]" $
        getBiggestDistance [Color 1 6 5] [Color 10 25 36] `shouldBe` sqrt 1403.0
    it "Should return the biggest distance between colors : [(1, 6, 5), (10, 25, 36)] and [(2, 7, 6), (10, 25, 36)]" $
        getBiggestDistance [Color 1 6 5, Color 10 25 36] [Color 2 7 6, Color 10 25 36] `shouldBe` sqrt 3.0

getClosestColorSpec :: Spec
getClosestColorSpec = describe "Function getClosestColor :" $ do
    it "Should return the closest color to pixel : (0, 0, (0, 0, 0)) in [(0, 0, 0), (0, 0, 0), (0, 0, 0)]" $
        getClosestColor (Pixel 0 0 (Color 0 0 0)) [Color 0 0 0, Color 0 0 0, Color 0 0 0] `shouldBe` Color 0 0 0
    it "Should return the closest color to pixel : (0, 0, (0, 0, 0)) in [(1, 1, 1), (1, 1, 1), (1, 1, 1)]" $
        getClosestColor (Pixel 0 0 (Color 0 0 0)) [Color 1 1 1, Color 1 1 1, Color 1 1 1] `shouldBe` Color 1 1 1
    it "Should return the closest color to pixel : (0, 0, (0, 0, 0)) in [(1, 0, 0), (1, 0, 0), (1, 0, 0)]" $
        getClosestColor (Pixel 0 0 (Color 0 0 0)) [Color 1 0 0, Color 1 0 0, Color 1 0 0] `shouldBe` Color 1 0 0
    it "Should return the closest color to pixel : (10, 10, (10, 10, 10)) in [(10, 10, 10), (10, 10, 10), (10, 10, 10)]" $
        getClosestColor (Pixel 10 10 (Color 10 10 10)) [Color 10 10 10, Color 10 10 10, Color 10 10 10] `shouldBe` Color 10 10 10
    it "Should return the closest color to pixel : (10, 10, (10, 10, 10)) in [(0, 0, 0), (0, 0, 0), (0, 0, 0)]" $
        getClosestColor (Pixel 10 10 (Color 10 10 10)) [Color 0 0 0, Color 0 0 0, Color 0 0 0] `shouldBe` Color 0 0 0
    it "Should return the closest color to pixel : (10, 10, (10, 10, 10)) in [(0, 0, 0), (1, 1, 1), (2, 2, 2)]" $
        getClosestColor (Pixel 10 10 (Color 10 10 10)) [Color 0 0 0, Color 1 1 1, Color 2 2 2] `shouldBe` Color 2 2 2
    it "Should return the closest color to pixel : (10, 10, (10, 10, 10)) in [(0, 0, 0), (1, 1, 1), (10, 10, 10)]" $
        getClosestColor (Pixel 10 10 (Color 10 10 10)) [Color 0 0 0, Color 1 1 1, Color 10 10 10] `shouldBe` Color 10 10 10

filterPixelsSpec :: Spec
filterPixelsSpec = describe "Function filterPixels :" $ do
    it "Should return the pixels of the colors list : [(0, 0, (0, 0, 0)), (1, 1 (1, 1, 1)), (2, 2, (2, 2, 2))] that are not in [(0, 0, (0, 0, 0)), (3, 3, (3, 3, 3))]" $
        filterPixels [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2)] [Pixel 0 0 (Color 0 0 0), Pixel 3 3 (Color 3 3 3)] `shouldBe` [Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2)]
    it "Should return the pixels of the colors list : [(0, 0, (0, 0, 0)), (1, 1 (1, 1, 1)), (2, 2, (2, 2, 2))] that are not in [(0, 0, (0, 0, 0)), (1, 1, (1, 1, 1)), (2, 2, (2, 2, 2))]" $
        filterPixels [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2)] [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2)] `shouldBe` []
    it "Should return the pixels of the colors list : [(0, 0, (0, 0, 0)), (1, 1 (1, 1, 1)), (2, 2, (2, 2, 2))] that are not in [(3, 3, (3, 3, 3))]" $
        filterPixels [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2)] [Pixel 3 3 (Color 3 3 3)] `shouldBe` [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 1 1 1), Pixel 2 2 (Color 2 2 2)]
    it "Should return the pixels of the colors list : [] that are not in [(0, 0, (0, 0, 0)), (3, 3, (3, 3, 3))]" $
        filterPixels [] [Pixel 0 0 (Color 0 0 0), Pixel 3 3 (Color 3 3 3)] `shouldBe` []

getOnlyNearestPixelsSpec :: Spec
