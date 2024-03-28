{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- ParseFileSpecs
-}

module ParseFileSpecs (
    parseFileSpecs
) where

import Test.Hspec
import OurData (
        Color(..),
        Pixel(..)
    )
import ParseFile (
        readReadColor,
        readReadPoint,
        checkReadPixel,
        readReadPixel,
        convertReadPixel,
        openFile,
        parseFile
    )

parseFileSpecs :: Spec
parseFileSpecs = describe "ParseFile Functions :" $ do
    readReadColorSpec
    readReadPointSpec
    checkReadPixelSpec
    readReadPixelSpec
    convertReadPixelSpec
    openFileSpec
    parseFileSpec

readReadColorSpec :: Spec
readReadColorSpec = describe "Function readReadColor :" $ do
    it "should read '(0, 0, 0)' and return Just (0, 0, 0)" $
        readReadColor "(0, 0, 0)" `shouldBe` Just (0, 0, 0)
    it "should read '(255, 255, 255)' and return Just (255, 255, 255)" $
        readReadColor "(255, 255, 255)" `shouldBe` Just (255, 255, 255)
    it "should read '(255, 0, 0)' and return Just (255, 0, 0)" $
        readReadColor "(255, 0, 0)" `shouldBe` Just (255, 0, 0)
    it "should read '(0, 255, 0)' and return Just (0, 255, 0)" $
        readReadColor "(0, 255, 0)" `shouldBe` Just (0, 255, 0)
    it "should read '(0, 0, 255)' and return Just (0, 0, 255)" $
        readReadColor "(0, 0, 255)" `shouldBe` Just (0, 0, 255)
    it "should read '(0, 0, 0, 0)' and return Nothing" $
        readReadColor "(0, 0, 0, 0)" `shouldBe` Nothing
    it "should read '(0, 0, 0' and return Nothing" $
        readReadColor "(0, 0, 0" `shouldBe` Nothing
    it "should read '0, 0, 0)' and return Nothing" $
        readReadColor "0, 0, 0)" `shouldBe` Nothing

readReadPointSpec :: Spec
readReadPointSpec = describe "Function readReadPoint :" $ do
    it "should read '(0, 0)' and return Just (0, 0)" $
        readReadPoint "(0, 0)" `shouldBe` Just (0, 0)
    it "should read '(255, 255)' and return Just (255, 255)" $
        readReadPoint "(255, 255)" `shouldBe` Just (255, 255)
    it "should read '(255, 0)' and return Just (255, 0)" $
        readReadPoint "(255, 0)" `shouldBe` Just (255, 0)
    it "should read '(0, 255)' and return Just (0, 255)" $
        readReadPoint "(0, 255)" `shouldBe` Just (0, 255)
    it "should read '(0, 0)' and return Just (0, 0)" $
        readReadPoint "(0, 0)" `shouldBe` Just (0, 0)
    it "should read '(0, 0, 0)' and return Nothing" $
        readReadPoint "(0, 0, 0)" `shouldBe` Nothing
    it "should read '(0, 0' and return Nothing" $
        readReadPoint "(0, 0" `shouldBe` Nothing
    it "should read '0, 0)' and return Nothing" $
        readReadPoint "0, 0)" `shouldBe` Nothing

checkReadPixelSpec :: Spec
checkReadPixelSpec = describe "Function checkReadPixel :" $ do
    it "should check (0, 0) (0, 0, 0) and return Just (0, 0) (0, 0, 0)" $
        checkReadPixel ((0, 0), (0, 0, 0)) `shouldBe` Just ((0, 0), (0, 0, 0))
    it "should check (0, 0) (255, 255, 255) and return Just (0, 0) (255, 255, 255)" $
        checkReadPixel ((0, 0), (255, 255, 255)) `shouldBe` Just ((0, 0), (255, 255, 255))
    it "should check (0, 0) (255, 0, 0) and return Just (0, 0) (255, 0, 0)" $
        checkReadPixel ((0, 0), (255, 0, 0)) `shouldBe` Just ((0, 0), (255, 0, 0))
    it "should check (0, 0) (0, 255, 0) and return Just (0, 0) (0, 255, 0)" $
        checkReadPixel ((0, 0), (0, 255, 0)) `shouldBe` Just ((0, 0), (0, 255, 0))
    it "should check (0, 0) (0, 0, 255) and return Just (0, 0) (0, 0, 255)" $
        checkReadPixel ((0, 0), (0, 0, 255)) `shouldBe` Just ((0, 0), (0, 0, 255))
    it "should check (0, 0) (0, 0, 256) and return Nothing" $
        checkReadPixel ((0, 0), (0, 0, 256)) `shouldBe` Nothing
    it "should check (0, 0) (0, 0, -1) and return Nothing" $
        checkReadPixel ((0, 0), (0, 0, -1)) `shouldBe` Nothing
    it "should check (0, 0) (0, 256, 0) and return Nothing" $
        checkReadPixel ((0, 0), (0, 256, 0)) `shouldBe` Nothing
    it "should check (0, 0) (0, -1, 0) and return Nothing" $
        checkReadPixel ((0, 0), (0, -1, 0)) `shouldBe` Nothing

readReadPixelSpec :: Spec
readReadPixelSpec = describe "Function readReadPixel :" $ do
    it "should read '(0,0) (0,0,0)' and return Just (0, 0) (0, 0, 0)" $
        readReadPixel "(0,0) (0,0,0)" `shouldBe` Just ((0, 0), (0, 0, 0))
    it "should read '(255,255) (255,255,255)' and return Just (255, 255) (255, 255, 255)" $
        readReadPixel "(255,255) (255,255,255)" `shouldBe` Just ((255, 255), (255, 255, 255))
    it "should read '(255,0) (255,0,0)' and return Just (255, 0) (255, 0, 0)" $
        readReadPixel "(255,0) (255,0,0)" `shouldBe` Just ((255, 0), (255, 0, 0))
    it "should read '(0,255) (0,255,0)' and return Just (0, 255) (0, 255, 0)" $
        readReadPixel "(0,255) (0,255,0)" `shouldBe` Just ((0, 255), (0, 255, 0))
    it "should read '(0,0) (0,0,255)' and return Just (0, 0) (0, 0, 255)" $
        readReadPixel "(0,0) (0,0,255)" `shouldBe` Just ((0, 0), (0, 0, 255))
    it "should read '(0,0) (0,0,0,0)' and return Nothing" $
        readReadPixel "(0,0) (0,0,0,0)" `shouldBe` Nothing
    it "should read '(0,0) (0,0,0' and return Nothing" $
        readReadPixel "(0,0) (0,0,0" `shouldBe` Nothing
    it "should read '(0,0) 0,0,0)' and return Nothing" $
        readReadPixel "(0,0) 0,0,0)" `shouldBe` Nothing
    it "should read '(0,0) (0,0,0) (0,0)' and return Nothing" $
        readReadPixel "(0,0) (0,0,0) (0,0)" `shouldBe` Nothing

convertReadPixelSpec :: Spec
convertReadPixelSpec = describe "Function convertReadPixel :" $ do
    it "should convert ((0, 0), (0, 0, 0)) and return Pixel 0 0 (Color 0 0 0)" $
        convertReadPixel ((0, 0), (0, 0, 0)) `shouldBe` Pixel 0 0 (Color 0 0 0)
    it "should convert ((255, 255), (255, 255, 255)) and return Pixel 255 255 (Color 255 255 255)" $
        convertReadPixel ((255, 255), (255, 255, 255)) `shouldBe` Pixel 255 255 (Color 255 255 255)
    it "should convert ((255, 0), (255, 0, 0)) and return Pixel 255 0 (Color 255 0 0)" $
        convertReadPixel ((255, 0), (255, 0, 0)) `shouldBe` Pixel 255 0 (Color 255 0 0)
    it "should convert ((0, 255), (0, 255, 0)) and return Pixel 0 255 (Color 0 255 0)" $
        convertReadPixel ((0, 255), (0, 255, 0)) `shouldBe` Pixel 0 255 (Color 0 255 0)
    it "should convert ((0, 0), (0, 0, 255)) and return Pixel 0 0 (Color 0 0 255)" $
        convertReadPixel ((0, 0), (0, 0, 255)) `shouldBe` Pixel 0 0 (Color 0 0 255)

openFileSpec :: Spec
openFileSpec = describe "Function openFile :" $ do
    it "should open the file and return the content" $ do
        result <- openFile "./test/TestsFiles/test.txt"
        result `shouldBe` "Hello, World!\n"
    it "should return an error message" $ do
        openFile "./test/TestsFiles/invalid.txt" `shouldThrow` anyException

parseFileSpec :: Spec
parseFileSpec = describe "Function parseFile :" $ do
    it "should parse the file and return the list of pixels" $ do
        result <- parseFile "./test/TestsFiles/PixelsTest.txt"
        result `shouldBe` [Pixel 0 0 (Color 0 0 0), Pixel 1 1 (Color 255 255 255), Pixel 2 2 (Color 255 0 0), Pixel 3 3 (Color 0 255 0), Pixel 4 4 (Color 0 0 255)]
    it "should parse the file and return an error from an invalid pixel" $ do
        parseFile "./test/TestsFiles/InvalidPixel.txt" `shouldThrow` anyException
    it "should return an error message" $ do
        parseFile "./test/TestsFiles/invalid.txt" `shouldThrow` anyException
