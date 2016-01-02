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
    _ <- run $ "pacmd set-sink-volume " ++ show s ++ " " ++ tar
    return ()

sinkVol :: [String] -> [String]
sinkVol lns = map (=~ "[0-9]*%") sinks
    where sinks = filter (=~ "^Sink") lns

sinksToIndices :: [String] -> [Int]
sinksToIndices sx = readMany $ concat parts
        where parts = filter (=~ "index") sx

main = do
    args <- getArgs
    sinkInfo <- run "pacmd list-sinks"

    if null args then do
        sinkVols <- run "pacmd dump-volumes"
        let vols = sinkVol . lines $ sinkVols
        putStrLn "Current sink volumes:"
        mapM_ putStrLn vols
    else do
        let setter = setVol $ read . head $ args
        let indices = sinksToIndices (lines sinkInfo)
        mapM_ setter indices

