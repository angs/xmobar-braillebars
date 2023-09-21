module BrailleBar (brailleLine, intToBraille, paddedUnary) where

--Expects a list of four lists with common length
brailleLine :: [[Int]] -> String
brailleLine [ (a1:a2 :as) -- 1   8
            , (b1:b2 :bs) -- 2  16
            , (c1:c2 :cs) -- 4  32
            , (d1:d2 :ds) --64 128
            ] = (toEnum $ 10240 + (sum $ zipWith (*) [a1,b1,c1,d1,a2,b2,c2,d2] [1,2,4,64,8,16,32,128]) :: Char) : (brailleLine [as,bs,cs,ds])
brailleLine [(a:[]), (b:[]), (c:[]), (d:[])] = [toEnum $ 10240 + (sum $ zipWith (*) [a,b,c,d] [1,2,4,64]) :: Char]
brailleLine _ = []

intToBraille :: Int -> [Int] -> String
intToBraille withLength = brailleLine . map (paddedUnary withLength)

--max (k, n) ones right-padded to length k with zeroes
paddedUnary 0 _ = []
paddedUnary k 0 = replicate k 0
paddedUnary k n = 1 : paddedUnary (k-1) (n-1)
