{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- Main
-}

module Main (main) where

import Algorithm (
        applyKMeans,
        getFinalClusters
    )

import OurData (
        Color(..),
        Pixel(..)
    )

import ArgsParsing (
        Options(..),
        parseArgs,
    )

import FileParsing (
        parseFile
    )

import Utils (
        getRandomColor,
        getFailingColor
    )

import System.Exit (exitWith, ExitCode(..))

displayResult:: [(Color, [Pixel])] -> IO ()
displayResult [] = return ()
displayResult (a : as) = putStrLn "--" >>
                        putStrLn (show (fst a)) >>
                        putStrLn "-" >>
                        mapM_ (putStrLn . show) (snd a) >>
                        displayResult as

main :: IO ()
main = do
    Options {oNumber = Just n, oLimit = Just l, oPath = Just p} <- parseArgs
    rPixels <- parseFile p
    if length rPixels < n
        then putStrLn "Error: not enough pixels" >>
            exitWith (ExitFailure 84)
    else let randColors = getRandomColor rPixels n
             colors = applyKMeans rPixels randColors (getFailingColor n) l
         in displayResult (getFinalClusters rPixels colors colors)
