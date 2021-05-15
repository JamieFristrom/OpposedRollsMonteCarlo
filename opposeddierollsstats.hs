import Data.List
import System.Environment
import System.Random


rollDice :: StdGen -> Int -> Int -> [Int]
rollDice gen numDice numSides = take numDice (randomRs (1,numSides) gen)

betterRoll :: [Int] -> [Int] -> Bool
betterRoll [_] [] = True
betterRoll [] [_] = False
betterRoll roll1 roll2
    | roll1 !! 0 > roll2 !! 0 = True
    | roll1 !! 0 < roll2 !! 0 = False
    | otherwise = betterRoll (drop 1 roll1) (drop 1 roll2)

main = do
    putStrLn "Executed"
    gen <- getStdGen
    args <- getArgs
    let numDice = read (args !! 0) :: Int
    let numIterations = read (args !! 1) :: Int
    rollOff gen numDice numIterations

rollOff :: StdGen -> Int -> Int -> IO ()
rollOff gen numDice numIterations = do
    let firstRoll = reverse $ sort $ rollDice gen numDice 6
    nextGen <- newStdGen
    let secondRoll = reverse $ sort $ rollDice nextGen numDice 6
    putStrLn $ "First roll "++ (show firstRoll)
    putStrLn $ "Second roll "++ (show secondRoll)
    putStrLn $ if (betterRoll firstRoll secondRoll) then "First Wins" else "First Loses"

    