divide :: Int -> Int -> Bool
divide m n = n `mod` m = = 0

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], divide x n]


mcd :: Int -> Int -> Int
mcd x y = maximum . filter (`divide` y) . divisores $ x

isqrt :: Int -> Int
isqrt n = floor (sqrt (fromIntegral n))


trail :: Int -> String -> String
trail n = unlines . reverse . take n . reverse . lines

multPairs:: Num a => [(a,a)] -> [a]
multPairs = map (uncurry (*))

fact n | n = = 0 = 1
       | otherwise = n * fact (n - 1)