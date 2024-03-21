{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- HandleArgs
-}

module HandleArgs (parseArgs) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode(..))
import System.Directory (doesFileExist)

data Options = Options {
    filePath :: Maybe String,
    colorNumber :: Maybe Int,
    convergenceLim :: Maybe Float
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    filePath = Nothing,
    colorNumber = Nothing,
    convergenceLim = Nothing
}

option :: [OptDescr (Options -> Options)]
option = [
    Option ['f'] [""] (ReqArg (\f opts -> opts {filePath = Just f}) "F")
        "path to the file containing the colors of the pixels",
    Option ['l'] [""] (ReqArg (\l opts -> opts
        {convergenceLim = readMaybe l}) "L") "convergence limit",
    Option ['n'] [""] (ReqArg (\n opts -> opts
        {colorNumber = readMaybe n}) "N") "number of colors in the final image"
    ]

help :: String
help = usageInfo "USAGE: ./imageCompressor -n N -l L -f F\n" option

parseArgs :: IO Options
parseArgs = getArgs >>= \args -> case getOpt Permute option args of
        (o, [], []) -> do
            opts <- foldl (>>=) (return defaultOptions) (map (return .) o)
            case (filePath opts, colorNumber opts, convergenceLim opts) of
                (Just f, Just _, Just _) -> do
                    exist <- doesFileExist f
                    if exist then return opts else putStrLn "File not exist" >>
                        exitWith (ExitFailure 84)
                _ -> putStrLn help >> exitWith (ExitFailure 84)
        (_, _, _) -> putStrLn help >> exitWith (ExitFailure 84)

