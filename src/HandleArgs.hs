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
import System.IO (hPutStrLn, stderr)

data Options = Options {
        oNumber :: Maybe Int,
        oLimit :: Maybe Double,
        oPath :: Maybe String
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    oNumber = Nothing,
    oLimit = Nothing,
    oPath = Nothing
}

options :: [OptDescr (Options -> Options)]
options = [
    Option ['f'] [""] (ReqArg (\f opts -> opts {oPath = Just f}) "F")
        "path to the file containing the colors of the pixels",
    Option ['l'] [""] (ReqArg (\l opts -> opts
        {oLimit = readMaybe l}) "L") "convergence limit",
    Option ['n'] [""] (ReqArg (\n opts -> opts
        {oNumber = readMaybe n}) "N") "number of colors in the final image"
    ]

getHelp :: String
getHelp = usageInfo "USAGE: ./imageCompressor -n N -l L -f F\n" options

checkArgs :: Options -> Maybe Options
checkArgs Options { oNumber = Nothing } = Nothing
checkArgs Options { oLimit = Nothing } = Nothing
checkArgs Options { oPath = Nothing } = Nothing
checkArgs opts = Just opts

parseOptions :: [String] -> IO Options
parseOptions args = case getOpt Permute options args of
    (o, [], []) -> foldl (>>=) (return defaultOptions) (map (return .) o)
    (_, _, errs) -> hPutStrLn stderr (concat errs) >>
        hPutStrLn stderr getHelp >>
        exitWith (ExitFailure 84)

parseArgs :: IO Options
parseArgs = do
    args <- getArgs
    opts <- parseOptions args
    case checkArgs opts of
        Just finalOpts -> return finalOpts
        Nothing -> hPutStrLn stderr getHelp >>
            exitWith (ExitFailure 84)
