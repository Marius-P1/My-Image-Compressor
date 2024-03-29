{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- HandleArgsSpecs
-}

module HandleArgsSpecs (
    handleArgsSpecs
) where

import Test.Hspec
import System.Environment (withArgs)
import HandleArgs (
        Options(..),
        defaultOptions,
        getHelp,
        checkArgs,
        parseArgs
    )

handleArgsSpecs :: Spec
handleArgsSpecs = describe "HandleArgs Functions :" $ do
    defaultOptionsSpec
    optionsSpec
    getHelpSpec
    checkArgsSpec
    dataOptionsSpec

defaultOptionsSpec :: Spec
defaultOptionsSpec = describe "Function defaultOptions :" $ do
    it "should return default options" $
        defaultOptions `shouldBe` Options {
            oNumber = Nothing,
            oLimit = Nothing,
            oPath = Nothing
        }

optionsSpec :: Spec
optionsSpec = describe "Function options :" $ do
    it "should return options in correct order" $ do
        let args = ["-n", "5", "-l", "0.5", "-f", "file.txt"]
        opt <- withArgs args parseArgs
        opt `shouldBe` Options (Just 5) (Just 0.5) (Just "file.txt")
    it "should return options in random order" $ do
        let args = ["-f", "file.txt", "-n", "5", "-l", "0.5"]
        opt <- withArgs args parseArgs
        opt `shouldBe` Options (Just 5) (Just 0.5) (Just "file.txt")
    it "should throw an error from incorrect options" $ do
        let args = ["-d", "5", "-l", "0.5", "-f", "file.txt"]
        withArgs args parseArgs `shouldThrow` anyException
    it "should throw an error from incorrect options" $ do
        let args = ["-n", "5", "-l", "0.5", "-f", "file.txt", "-d"]
        withArgs args parseArgs `shouldThrow` anyException
    it "should throw an error from missing options" $ do
        let args = ["-n", "5", "-l", "0.5"]
        withArgs args parseArgs `shouldThrow` anyException


getHelpSpec :: Spec
getHelpSpec = describe "Function getHelp :" $ do
    it "should return help message" $
        getHelp `shouldBe` "USAGE: ./imageCompressor -n N -l L -f F\n\n\
            \  -f F  --=F  path to the file containing the colors of the pixels\n\
            \  -l L  --=L  convergence limit\n\
            \  -n N  --=N  number of colors in the final image\n"

checkArgsSpec :: Spec
checkArgsSpec = describe "Function checkArgs :" $ do
    it "should return Nothing" $
        checkArgs Options { oNumber = Nothing, oLimit = Just 0.1, oPath = Just "file.txt" } `shouldBe` Nothing
    it "should return Nothing" $
        checkArgs Options { oNumber = Just 3, oLimit = Nothing, oPath = Just "file.txt" } `shouldBe` Nothing
    it "should return Nothing" $
        checkArgs Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Nothing } `shouldBe` Nothing
    it "should return Just Options" $
        checkArgs Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" } `shouldBe` Just Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" }

dataOptionsSpec :: Spec
dataOptionsSpec = describe "Data Options :" $ do
    it "should return Options" $
        Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" } `shouldBe` Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" }
    it "should show Options" $
        show (Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" }) `shouldBe` "Options {oNumber = Just 3, oLimit = Just 0.1, oPath = Just \"file.txt\"}"
    describe "Options accessors" $ do
        it "should return correct number value" $
            let opt = Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" }
            in oNumber opt `shouldBe` Just 3
        it "should return correct limit value" $
            let opt = Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" }
            in oLimit opt `shouldBe` Just 0.1
        it "should return correct path value" $
            let opt = Options { oNumber = Just 3, oLimit = Just 0.1, oPath = Just "file.txt" }
            in oPath opt `shouldBe` Just "file.txt"
