module Vigenere where

import Data.List
import Data.Char
import Control.Monad

-----------------------
-- Encoding/Decoding strings

alphaOrder :: Char -> Int
alphaOrder x = ord x - ord 'a'

alphaChar :: Int -> Char
alphaChar n = chr $ 97 + n

encode :: String -> [Int]
encode = map alphaOrder

decode :: [Int] -> String
decode = map alphaChar

----------------------------
-- Encryption/Decryption

shift :: Int -> Int -> Int
shift d n = (d+n) `mod` 26

-- Key -> Cleartext -> Ciphertext
encrypt :: [Int] -> [Int] -> [Int]
encrypt key = zipWith shift (cycle key)

decrypt :: [Int] -> [Int] -> [Int]
decrypt key = encrypt $ map (\n -> (-n) `mod` 26) key

-----------------------------
-- Constants for frequency analysis

alphaProb :: [Double]
alphaProb = (/100) <$> [8.15, 1.44, 2.76, 3.79, 13.11, 2.92, 1.99, 5.26, 6.35, 0.13, 0.42, 3.39, 2.54, 7.10, 8.00, 1.98, 0.12, 6.83, 6.10, 10.47, 2.46, 0.92, 1.54, 0.17, 1.98, 0.08]


--------------------------------
-- (Slow and NAIVE) Bayesian analysis

prob :: [Int] -> Double
prob str = product $ map (\n -> alphaProb !! n) str

-- Key 
probGivenKey :: [Int] -> [Int] -> Double
probGivenKey key str = prob $ decrypt key str

-- Ciphertext -> Prob
totalProb :: Int -> [Int]  -> Double
totalProb n e = (1/26)^n * (sum $ map (\k -> probGivenKey k e) $ keys n)

likelihoodGivenKey :: Int -> [Int] ->  [Int] -> Double
likelihoodGivenKey n key str = probGivenKey key str / totalProb n str

keys :: Int -> [[Int]]
keys n = replicateM n [0..25]

----------------------------------------
-- Frequency analysis tools based on index of coincidence and mutual index of coincidence

count :: (Eq a) => [a] -> a -> Int
count list x = length . filter (==x) $ list

ic :: [Int] -> Double
ic str = fromIntegral (sum . map (\c -> c*(c-1)) $ freq) / fromIntegral (n*(n-1))
  where freq = map (count str) [0..25]
        n = length str

icNorm :: Double
icNorm = sum . map (\p -> p^2) $ alphaProb

icForKeyLength :: Int -> [Int] -> Double
icForKeyLength n str = average $ map ic $ transpose $ split n str

split :: Int -> [a] -> [[a]]
split _ [] = []
split n str = take n str : split n (drop n str)

average :: [Double] -> Double
average freq = sum freq / (fromIntegral (length freq))

-- Max key length -> Ciphertext -> List of IC
icList :: Int -> [Int] -> [(Int,Double)]
icList n str = map (\n -> (n, icForKeyLength n str)) [1..n]

-- Max key length -> Ciphertext -> List of IC diff (from IC norm)
icDiffList :: Int -> [Int] -> [(Int,Double)]
icDiffList n = map (\(n,ic) -> (n,abs (ic-icNorm))) . icList n

freqs :: [Int] -> [Double]
freqs str = (/ fromIntegral (length str)) <$> map (fromIntegral . count str) [0..25] 

mc :: [Int] -> [Int] -> Double
mc str1 str2 = sum $ zipWith (*) (freqs str1) (freqs str2)

mcNorm :: Double
mcNorm = icNorm

mcList :: [Int] -> [Int] -> [(Int,Double)]
mcList str1 str2 = zip [0..25] $ mc str1 <$> map (\n -> map (shift (-n)) str2) [0..25]

mcDiffList :: [Int] -> [Int] -> [(Int,Double)]
mcDiffList str1 str2 = map (\(n,mc) -> (n,abs (mc-mcNorm))) $ mcList str1 str2

-----------------------------------------------
-- Finding probable key length

-- | standard extendedGCD algorithm
-- gives result in (a,b,d), an+bm=d
extendedGCD :: Int -> Int -> (Int,Int,Int)
extendedGCD a b = if b==0
                  then (1,0,a)
                  else (y,x-(a `div` b)*y,d)
  where (x,y,d) = extendedGCD b (a `mod` b)

gcdOver :: [Int] -> Int
gcdOver (x:[]) = x
gcdOver (x:xs) = gcd x (gcdOver xs)
  where gcd a b = (\(_,_,d) -> d) $ extendedGCD a b


possibleMultiples :: [(Int,Double)] -> [Int]
possibleMultiples = map fst . filter ((<0.01) . snd) 

-- Max keylength -> Ciphertext -> Probable key
probableLength :: Int -> [Int] -> Int
probableLength n str = gcdOver $ possibleMultiples $ sortOn snd $ icDiffList n str


-----------------------------------------
-- Finding probable gap between key columns (using mutual index of coincidence)

trd :: (a,b,c) -> c
trd (_,_,c) = c

mutualIndices :: Int -> [(Int,Int)]
mutualIndices 2 = [(0,1)]
mutualIndices n = ((\m -> (m,n-1)) <$> [0..n-2]) ++ mutualIndices (n-1)

mcDiffListAll :: Int -> [Int] -> [(Int,Int,[(Int,Double)])]
mcDiffListAll n str = map (\(n,m) -> (n,m,mcDiffList (col!!n) (col!!m))) $ mutualIndices n
  where col = transpose $ split n str

processDiffMC :: [(Int,Int,[(Int,Double)])] -> [(Int,Int,[(Int,Double)])]
processDiffMC = map (\(n,m,list) -> (n,m, sortOn snd $ filter ((<0.01) . snd) list))

probableDisplacements :: Int -> [Int] -> [Int]
probableDisplacements n str = [0] ++ (
                              map snd . sort . map (\(_,m,displacement:_) -> (m,fst displacement)) . filter (\(n,_,_) -> n==0) . processDiffMC . mcDiffListAll n $ str )


------------------------------------------
-- Finding the systematic rotation of all columns (the rotation of column 1)

-- Shift amount -> Key displacemens -> Ciphertext -> Cleartext
rotDecrypt :: Int -> [Int] -> [Int] -> [Int]
rotDecrypt n key = decrypt (map (shift n) key)

sumProbDiff :: [Int] -> Double
sumProbDiff str = sum . map abs . zipWith (-) (freqs str) $ alphaProb

rotProbDiffList :: [Int] -> [Int] -> [(Int,Double)]
rotProbDiffList key str = map (\n -> (n,sumProbDiff $ rotDecrypt n key str)) [0..25]

--------------------------------------------
leastDiffShift :: [(Int,Double)] -> Int
leastDiffShift = fst . head . sortOn snd

probableKey :: [Int] -> [Int]
probableKey str = map (shift (leastDiffShift dat1)) displacements
  where
    dat1 = rotProbDiffList displacements str
    displacements = probableDisplacements key_length str
    key_length = probableLength 20 str

probableCleartext :: [Int] -> [Int]
probableCleartext str = decrypt (probableKey str) str
