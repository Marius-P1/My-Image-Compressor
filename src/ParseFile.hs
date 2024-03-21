{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- ParseFile
-}

module ParseFile (parseFile) where

import OurData (
    Color(..),
    Pixel(..)
    )

import Data.Maybe (catMaybes, isJust)
import Control.Exception (try, SomeException)
import System.Exit (exitWith, ExitCode(..))

type ReadPosition = (Int, Int)
type ReadColor = (Int, Int, Int)

readReadColor :: String -> Maybe ReadColor
readReadColor s = case reads s of
    [(a, "")] -> Just a
    _ -> Nothing

readReadPoint :: String -> Maybe ReadPosition
readReadPoint s = case reads s of
    [(a, "")] -> Just a
    _ -> Nothing

checkReadPixel :: (ReadPosition, ReadColor) -> Maybe (ReadPosition, ReadColor)
checkReadPixel (point, (r, g, b)) =
    if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255
        then Nothing
    else Just (point, (r, g, b))

readReadPixel :: String -> Maybe (ReadPosition, ReadColor)
readReadPixel s = case words s of
    (a : b : []) -> do
        rPoint <- readReadPoint a
        rColor <- readReadColor b
        checkReadPixel (rPoint, rColor)
    _ -> Nothing

convertReadPixel :: (ReadPosition, ReadColor) -> Pixel
convertReadPixel ((x1, y1), (r, g, b)) = Pixel x1 y1 (Color r g b)

openFile :: FilePath -> IO String
openFile filePath = do
    contents <- try (readFile filePath) :: IO (Either SomeException String)
    case contents of
        Left _ -> putStrLn "Error: invalid file" >>
            exitWith (ExitFailure 84)
        Right c -> return c

parseFile :: FilePath -> IO [Pixel]
parseFile filePath = do
    contents <- openFile filePath
    let fileLines = lines contents
        parsedLines = map readReadPixel fileLines
    if all isJust parsedLines
        then return $ map convertReadPixel $ catMaybes parsedLines
        else putStrLn "Error: invalid file" >> exitWith (ExitFailure 84)
