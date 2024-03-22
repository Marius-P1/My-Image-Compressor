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

import HandleArgs (
        Options(..),
        parseArgs,
    )

import ParseFile (
        parseFile
    )

import Utils (
        getRandomColor,
        getFailingColor
    )

import System.Exit (exitWith, ExitCode(..))
import System.Random (newStdGen)

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
    seed <- newStdGen
    if length rPixels < n
        then putStrLn "Error: not enough pixels" >>
            exitWith (ExitFailure 84)
    else let randColors = getRandomColor seed rPixels n
             colors = applyKMeans rPixels randColors (getFailingColor n) l
         in displayResult (getFinalClusters rPixels colors colors)
