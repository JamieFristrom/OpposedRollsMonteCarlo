import System.Environment
import System.Random

dieroll :: StdGen -> Int -> Int -> [Int]
dieroll gen numDice numSides = take numDice (randomRs (1, numSides) gen)

main = do
    gen <- getStdGen
    args <- getArgs
    let numDice = read (args !! 0) :: Int
    let numSides = read ( args !! 1) :: Int
    putStrLn $ "Rolling "++(show numDice)++"d"++(show numSides)
    let result = dieroll gen numDice numSides
    putStrLn $ show result
    putStrLn $ "Total: " ++ ( show ( sum result))