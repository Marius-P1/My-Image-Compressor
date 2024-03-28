{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- Spec
-}

import Test.Hspec

import AlgorithmSpecs (algorithmSpecs)
import HandleArgsSpecs (handleArgsSpecs)
import OurDataSpecs (ourDataSpecs)
import ParseFileSpecs (parseFileSpecs)
import UtilsSpecs (utilsSpecs)

main :: IO ()
main = hspec $ do
    algorithmSpecs
    handleArgsSpecs
    ourDataSpecs
    parseFileSpecs
    utilsSpecs
