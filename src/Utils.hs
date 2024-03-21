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

getMaxColor :: Color
getMaxColor = Color maxBound maxBound maxBound

getFailingColor :: Int -> [Color]
getFailingColor n = replicate n getMaxColor

getRandomColor :: [Pixel] -> Int -> [Color]
getRandomColor _ 0 = []
getRandomColor pixels n =
    let (Pixel _ _ colore) = pixels !! (n - 1)
    in colore : getRandomColor pixels (n - 1)
