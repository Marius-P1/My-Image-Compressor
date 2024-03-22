{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- Utils
-}

module Utils (
        getMaxColor,
        getFailingColor,
        getRandomColor
    ) where

import OurData (
        Color(..),
        Pixel(..)
    )

import System.Random (StdGen, randomR)

getMaxColor :: Color
getMaxColor = Color maxBound maxBound maxBound

getFailingColor :: Int -> [Color]
getFailingColor n = replicate n getMaxColor

getRandomColor :: StdGen -> [Pixel] -> Int -> [Color]
getRandomColor _ _ 0 = []
getRandomColor g0 pixels n =
    let (i, g1) = randomR (0, length pixels - 1) g0
        selectedPixel = pixels !! i
        restPixels = filter (/= selectedPixel) pixels
    in color selectedPixel : getRandomColor g1 restPixels (n - 1)
