{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- Spec
-}

import Test.Hspec


main :: IO ()
main = hspec $ do
    algorithmSpec
    handleArgsSpec
    ourDataSpec
    parseFileSpec
    utilsSpec
