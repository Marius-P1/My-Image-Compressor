{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- Algorithm
-}

module Algorithm (
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
    ) where

import OurData (
        Color(..),
        Pixel(..)
    )

computeDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
computeDistance (x1, y1, z1) (x2, y2, z2) =
    let dX = x2 - x1
        dY = y2 - y1
        dZ = z2 - z1
    in sqrt $ fromIntegral (dX * dX + dY * dY + dZ * dZ)

computeDistancePixelToColor :: Pixel -> Color -> Double
computeDistancePixelToColor (Pixel {color = Color x1 y1 z1}) (Color x2 y2 z2) =
    computeDistance (x1, y1, z1) (x2, y2, z2)

computeDistanceColorToColor :: Color -> Color -> Double
computeDistanceColorToColor (Color x1 y1 z1) (Color x2 y2 z2) =
    computeDistance (x1, y1, z1) (x2, y2, z2)

sumPixels :: [Pixel] -> Color
sumPixels pixels = foldr (+) (Color 0 0 0) (map color pixels)

computeCentroidsFromMeans :: [Pixel] -> Color
computeCentroidsFromMeans [] = Color 0 0 0
computeCentroidsFromMeans pixels =
    let Color r g b = sumPixels pixels
        len = length pixels
    in Color (r `div` len) (g `div` len) (b `div` len)

getBiggestDistance :: [Color] -> [Color] -> Double
getBiggestDistance [] [] = -1
getBiggestDistance news olds =
    maximum $ zipWith computeDistanceColorToColor news olds

getClosestColor :: Pixel -> [Color] -> Color
getClosestColor _ [] = Color maxBound maxBound maxBound
getClosestColor pixel (a : as) =
    let next = getClosestColor pixel as
        dist1 = computeDistancePixelToColor pixel a
        dist2 = computeDistancePixelToColor pixel next
    in if dist1 < dist2 then a else next

filterPixels :: [Pixel] -> [Pixel] -> [Pixel]
filterPixels [] _ = []
filterPixels (a : as) colors =
    let next = filterPixels as colors
    in if a `elem` colors then next else a : next

getOnlyNearestPixels :: [Pixel] -> Color -> [Color] -> [Pixel]
getOnlyNearestPixels [] _ _ = []
getOnlyNearestPixels (a : as) current colors =
    let closest = getClosestColor a colors
        next = getOnlyNearestPixels as current colors
    in if closest == current then a : next else next

getNewCentroids :: [Pixel] -> [Color] -> [Color] -> [Color]
getNewCentroids pixels [_] _ = [computeCentroidsFromMeans pixels]
getNewCentroids pixels (a : as) colors =
    let nearest = getOnlyNearestPixels pixels a colors
        filtered = filterPixels pixels nearest
    in computeCentroidsFromMeans nearest : getNewCentroids filtered as colors
getNewCentroids _ [] _ = []

applyKMeans :: [Pixel] -> [Color] -> [Color] -> Double -> [Color]
applyKMeans pixels colors oldColors limit =
    let distance = getBiggestDistance colors oldColors
        newColors = getNewCentroids pixels colors colors
    in if distance < limit
        then colors
        else applyKMeans pixels newColors colors limit

getFinalClusters :: [Pixel] -> [Color] -> [Color] -> [(Color, [Pixel])]
getFinalClusters pixels (a : as) colors =
    let nearest = getOnlyNearestPixels pixels a colors
    in (a, nearest) : getFinalClusters (filterPixels pixels nearest) as colors
getFinalClusters _ _ _ = []
