import System.IO

main :: IO ()
main = do
    handle <- openFile "Input.txt" ReadMode
    contents <- hGetContents handle
    let depths = map read . lines $ contents :: [Integer]
    print $ "Part 1: " ++ show (part1 depths)
    print $ "Part 2: " ++ show (part2 depths)
    hClose handle

depthDiffs :: [Integer] -> [Integer]
depthDiffs xs = zipWith (-) xs (tail xs)

depthIncreases :: [Integer] -> Integer
depthIncreases = foldr (\x xs -> if x < 0 then 1 + xs else xs) 0

part1 :: [Integer] -> Integer
part1 = depthIncreases . depthDiffs

depthTripleSums :: [Integer] -> [Integer]
depthTripleSums xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail . tail $ xs)

part2 :: [Integer] -> Integer
part2 = part1 . depthTripleSums
