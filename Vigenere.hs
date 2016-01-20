import Data.List
import Data.Char
import Control.Monad

-----------------------
-- Character encodings

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

prob :: [Int] -> Double
prob str = product $ map (\n -> alphaProb !! n) str

-- Key 
probGivenKey :: [Int] -> [Int] -> Double
probGivenKey key str = prob $ decrypt key str

-- Ciphertext -> Prob
totalProb :: [Int]  -> Double
totalProb e = (1/26)^3 * (sum $ map (\k -> probGivenKey k e) $ keys)

likelihoodGivenKey :: [Int] ->  [Int] -> Double
likelihoodGivenKey key str = probGivenKey key str / totalProb str

keys :: [[Int]]
keys = replicateM 3 [0..25]


count :: (Eq a) => [a] -> a -> Int
count list x = length . filter (==x) $ list

ic :: [Int] -> Double
ic str = fromIntegral (sum . map (\c -> c*(c-1)) $ freq) / fromIntegral (n*(n-1))
  where freq = map (count str) [0..25]
        n = length str

icNorm :: Double
icNorm = sum . map (\p -> p^2) $ alphaProb

--result = take 50 $ reverse $ sortOn fst $  map (\k -> ((ic $ decrypt k sample) - icNorm, k)) keys


icForKeyLength :: Int -> [Int] -> Double
icForKeyLength n str = average $ map ic $ transpose $ split n str

split _ [] = []
split n str = take n str : split n (drop n str)

average freq = sum freq / (fromIntegral (length freq))

icList :: [Int] -> [(Int,Double)]
icList str = map (\n -> (n, icForKeyLength n str)) [1..20]

icDiffList :: [Int] -> [(Int,Double)]
icDiffList = map (\(n,ic) -> (n,abs (ic-icNorm))) . icList

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


--------------------------------------

sample = encrypt (encode "key") $ encode "therearemorethingsinheavenandearthhoratiothanyouhavedreamtofinyourphilosophy"

sample1 = encode "therearemorethingsinheavenandearthhoratiothanyouhavedreamtofinyourphilosophy"

sample2 = encode $ map toLower $ "WZVFINLSOHCWAQMERTBJAHIHVPEIEHZCSVMKEIAJXMUHVJTSGHNCAWLVMAANWBQRJYLVVQCBBWLZYGGRZCBQGVZRGZEQRVLVSAQSASCHHZYTBWDSORSBSEEVEGGHVNLSEHWRVQKSFTVWDOQQSGTCGXNSFRVTZNIHNGNWMFYSVQEHNQHNSAGLOHUHYJPOSDXCBNXYZUTKPOYLGVHIGKKIGSMTEUEHOCEFSEGEEVWHVRRJZSUHSOFFSEDIQHNWAJMESEERSBZLRULSJHHZNVWYPCBXHRSRVKSEURPRNBQROEUHNTRHPMPRLVHSRSCRYDFWQDVGAYPTUHNHUHTCPAFXNSBIQRVIAJWRNLWPNHNLJKBXPUMEJRNHUWLVERBXXZRRJXPTGLJUHSEEOPVFGWAJXYPDNLOWRVAYPNFXZRRQPPLWULPSEDFSTTJLPVCLRBPYRVNOAFPFDEOBD"

sample3 = encode $ map toLower $ "LIVITCSWPIYVEWHEVSRIQMXLEYVEOIEWHRXEXIPFEMVEWHKVSTYLXZIXLIKIIXPIJVSZEYPERRGERIMWQLMGLMXQERIWGPSRIHMXQEREKIETXMJTPRGEVEKEITREWHEXXLEXXMZITWAWSQWXSWEXTVEPMRXRSJGSTVRIEYVIEXCVMUIMWERGMIWXMJMGCSMWXSJOMIQXLIVIQIVIXQSVSTWHKPEGARCSXRWIEVSWIIBXVIZMXFSJXLIKEGAEWHEPSWYSWIWIEVXLISXLIVXLIRGEPIRQIVIIBGIIHMWYPFLEVHEWHYPSRRFQMXLEPPXLIECCIEVEWGISJKTVWMRLIHYSPHXLIQIMYLXSJXLIMWRIGXQEROIVFVIZEVAEKPIEWHXEAMWYEPPXLMWYRMWXSGSWRMHIVEXMSWMGSTPHLEVHPFKPEZINTCMXIVJSVLMRSCMWMSWVIRCIGXMWYMX"

sample4 = encode $ filter isLower . map toLower $ "In physics, a gauge theory is a type of field theory in which the Lagrangian is invariant under a continuous group of local transformations.The term gauge refers to redundant degrees of freedom in the Lagrangian. The transformations between possible gauges, called gauge transformations, form a Lie group—referred to as the symmetry group or the gauge group of the theory. Associated with any Lie group is the Lie algebra of group generators. For each group generator there necessarily arises a corresponding vector field called the gauge field. Gauge fields are included in the Lagrangian to ensure its invariance under the local group transformations (called gauge invariance). When such a theory is quantized, the quanta of the gauge fields are called gauge bosons. If the symmetry group is non-commutative, the gauge theory is referred to as non-abelian, the usual example being the Yang–Mills theory.Many powerful theories in physics are described by Lagrangians that are invariant under some symmetry transformation groups. When they are invariant under a transformation identically performed at every point in the space in which the physical processes occur, they are said to have a global symmetry. The requirement of local symmetry, the cornerstone of gauge theories, is a stricter constraint. In fact, a global symmetry is just a local symmetry whose group's parameters are fixed in space-time.Gauge theories are important as the successful field theories explaining the dynamics of elementary particles. Quantum electrodynamics is an abelian gauge theory with the symmetry group U(1) and has one gauge field, the electromagnetic four-potential, with the photon being the gauge boson. The Standard Model is a non-abelian gauge theory with the symmetry group U(1)×SU(2)×SU(3) and has a total of twelve gauge bosons: the photon, three weak bosons and eight gluons.Gauge theories are also important in explaining gravitation in the theory of general relativity. Its case is somewhat unique in that the gauge field is a tensor, the Lanczos tensor. Theories of quantum gravity, beginning with gauge gravitation theory, also postulate the existence of a gauge boson known as the graviton. Gauge symmetries can be viewed as analogues of the principle of general covariance of general relativity in which the coordinate system can be chosen freely under arbitrary diffeomorphisms of spacetime. Both gauge invariance and diffeomorphism invariance reflect a redundancy in the description of the system. An alternative theory of gravitation, gauge theory gravity, replaces the principle of general covariance with a true gauge principle with new gauge fields.Historically, these ideas were first stated in the context of classical electromagnetism and later in general relativity. However, the modern importance of gauge symmetries appeared first in the relativistic quantum mechanics of electrons – quantum electrodynamics, elaborated on below. Today, gauge theories are useful in condensed matter, nuclear and high energy physics among other subfields."

sample5 = encrypt [1,2,3,4,5] sample4
