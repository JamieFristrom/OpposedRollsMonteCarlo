import Data.List
import System.Environment
import System.Random


rollDice :: StdGen -> Int -> Int -> [Int]
rollDice gen numDice numSides = take numDice (randomRs (1,numSides) gen)

betterRoll :: [Int] -> [Int] -> Bool
betterRoll roll1 roll2
    | length roll2 == 0 = True
    | length roll1 == 0 = False
    | roll1 !! 0 > roll2 !! 0 = True
    | roll1 !! 0 < roll2 !! 0 = False
    | otherwise = betterRoll (drop 1 roll1) (drop 1 roll2)

rollOff :: [Int] -> Int -> Int -> ([Int], [Int])
rollOff diceList numDice1 numDice2 = (firstRoll, secondRoll)
    where firstRoll = reverse $ sort $ take numDice1 diceList
          secondRoll = reverse $ sort $ take numDice2 $ drop numDice1 diceList

main = do
    putStrLn "Executed"
    gen <- getStdGen
    args <- getArgs
    let numDice1 = read (args !! 0) :: Int
    let numDice2 = read (args !! 1 ) :: Int
    let numIterations = read (args !! 2) :: Int
    let diceList = randomRs (1, 6) gen
    let allRolls = [rollOff (drop (n*(numDice1+numDice2)) diceList) numDice1 numDice2 | n <- [0..numIterations]]
    --putStrLn $ show allRolls
    let wins = map (uncurry betterRoll) allRolls
    --putStrLn $ show wins
    let pct = fromIntegral( length (filter (==True) wins)) / (fromIntegral (length wins))
    putStrLn $ show pct