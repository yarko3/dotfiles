#!/usr/bin/env runhaskell
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Process
import Text.Regex.Posix

maxVol :: Float
maxVol = 65536

readMany = unfoldr $ listToMaybe . concatMap reads . tails

-- I love this hack. Security be damned.
run :: String -> IO String
run cmd = readProcess "sh" ["-c", cmd] ""

setVol :: Float -> Int -> IO ()
setVol t s = do
    let tar = show . round $ t / 100 * maxVol
    putStrLn $ "Setting sink " ++ show s ++ " volume to: " ++ show t ++ "% (" ++ tar ++ ")"
    _ <- run $ "pacmd set-sink-volume 0 " ++ tar
    return ()

volInfo :: [String] -> [String]
volInfo = map (=~ "0-9*]%")

sinksToIndices :: [String] -> [Int]
sinksToIndices sx = readMany $ concat parts
        where parts = filter (=~ "index") sx

main = do
    args <- getArgs
    sinkInfo <- run "pacmd list-sinks"
    let setter = setVol $ read . head $ args
    let indices = sinksToIndices (lines sinkInfo)
    mapM_ setter indices
