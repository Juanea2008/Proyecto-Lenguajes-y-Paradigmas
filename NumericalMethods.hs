-------------Useful Functions-------------
----1. Remove data outside an interval----
--Prototype
remData :: [Int] -> Int -> Int -> [Int]
--Body
remData [] _ _ = []
remData (x:xs) low high
  | x >= low && x <= high = x : remData xs low high
  | otherwise             = remData xs low high
  
----2. Order a list of Floats in descendant order----
--Prototype
orderDesc :: [Float] -> [Float]
--Body
orderDesc [] = []
orderDesc (p:xs) =
    orderDesc [x | x <- xs, x >= p] ++ [p] ++ orderDesc [x | x <- xs, x < p]

-------------Numerical methods-------------
----Functions: Pow, Factorial----
--Pow--
--Prototype
pow ::  Double -> Int -> Double
--Body
pow x n
  | n <= 0    = 1.0
  | otherwise = x * pow x (n - 1)

--Factorial--
--Prototype
factorial :: Int -> Double
--Body
factorial n
  | n <= 1    = 1.0
  | otherwise = fromIntegral n * factorial (n - 1)
  
----3. Approximation of exponential function----
--Prototype
expAprox :: Float -> Int -> Float
--Body
expAprox x n = auxExp x (toInteger n) 0 0.0
  where
    auxExp :: Float -> Integer -> Integer -> Float -> Float
    auxExp _ n k acc
      | k >= n    = acc
    auxExp x n k acc =
      let num  = pow (realToFrac x) (fromInteger k)   -- x^k
          den  = factorial (fromInteger k)           -- k!
          term = realToFrac (num / den)
      in auxExp x n (k+1) (acc + term)

---Exponential Percent Error--
--Prototype
expPctError :: Float -> Int -> Float
--Body
-- Ahora el error se calcula contra exp de Haskell
expPctError x n = 
  let aprox = expAprox x n
      real  = exp (realToFrac x)   -- función real de Haskell
  in 100.0 * abs ((real - realToFrac aprox) / real)

----4. Cosine approximation----
--Prototype
cosAprox :: Float -> Int -> Float
--Body
cosAprox x n = auxCos x (toInteger n) 0 0.0
  where
    auxCos :: Float -> Integer -> Integer -> Float -> Float
    auxCos _ n k acc
      | k >= n    = acc
    auxCos x n k acc =
      let num  = pow (realToFrac x) (fromInteger (2*k))   
          den  = factorial (fromInteger (2*k))            
          sign = if even k then 1.0 else -1.0             
          term = sign * realToFrac (num / den)            
      in auxCos x n (k + 1) (acc + term)

----Cosine Percent Error----
--Prototype
cosPctError :: Float -> Int -> Float
--Body
-- Ahora el error se calcula contra cos de Haskell
cosPctError x n =
  let approx = cosAprox x n
      real   = cos (realToFrac x)
  in 100.0 * abs ((real - realToFrac approx) / real)
  

----5. Approximation of natural logarithm ln(1+x)----
--Prototype
ln1pAprox :: Float -> Int -> Float
--Body
ln1pAprox x n = auxLn x (toInteger n) 1 0.0
  where
    auxLn :: Float -> Integer -> Integer -> Float -> Float
    auxLn _ n k acc
      | k > n     = acc
    auxLn x n k acc =
      let num  = pow (realToFrac x) (fromInteger k)       -- x^k Double
          sign = if even k then (-1.0) else 1.0           -- (-1)^(k+1)
          term = sign * (realToFrac num / fromIntegral k) -- Float
      in auxLn x n (k+1) (acc + term)

----Log(x+1) Percent Error----
--Prototype
ln1pPctError :: Float -> Int -> Float
--Body
-- Ahora el error se calcula contra log(1+x) de Haskell
ln1pPctError x n =
  let approx = ln1pAprox x n
      real   = log (1 + realToFrac x)
  in 100.0 * abs ((real - realToFrac approx) / real)
-------------Discrete signal processing techniques-------------
----6. Discrete Cosine Transform (DCT)----
--Factor de normalización a(k)--
aCoeff :: Int -> Int -> Float
aCoeff k n
  | k == 0    = sqrt (1.0 / fromIntegral n)
  | otherwise = sqrt (2.0 / fromIntegral n)

--Calcula un solo coeficiente X(k) de la DCT--
dctCoeff :: [Float] -> Int -> Float
dctCoeff xs k =
  let n = length xs
      ak = aCoeff k n
      sumVal = sum [ x * cos ( ((fromIntegral n' + 0.5) * pi * fromIntegral k) / fromIntegral n )
                   | (x, n') <- zip xs [0..] ]
  in ak * sumVal

--DCT completa de una lista de datos--
dct :: [Float] -> [Float]
dct xs =
  let n = length xs
  in [ dctCoeff xs k | k <- [0 .. n-1] ]
  

-------------Main-------------
main :: IO ()
main = do
  putStrLn "=== Tests prácticos ==="
  putStrLn "1) remData [1,25,5,-4] 0 5 -> debería dar [1,5]"
  print (remData [1,25 ,5 ,-4] 0 5)
  putStrLn ""

  putStrLn "2) orderDesc [3.5, 1.2, 7.8, 4.4, 2.0] -> deberia dar [7.8,4.4,3.5,2.0,1.2]"
  print (orderDesc [3.5, 1.2, 7.8, 4.4, 2.0])
  putStrLn ""

  putStrLn "3) expApprox 1.0 10 vs exp 1.0 (error %):"
  print (expAprox 1.0 10)
  print (expPctError 1.0 10)
  putStrLn ""

  putStrLn "4) cosApprox 1.5 6 vs cos 1.5 (error %):"
  print (cosAprox 1.5 6)
  print (cosPctError 1.5 6)
  putStrLn ""

  putStrLn "5) ln1pApprox 0.5 10 vs log(1.5) (error %):"
  print (ln1pAprox 0.5 10)
  print (ln1pPctError 0.5 10)
  putStrLn "" 
  
  putStrLn "6) DCT ejemplo [1..10] -> aproximación:"
  print (dct [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0])
