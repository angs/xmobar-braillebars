module BrailleBar (brailleBar) where

--Expects a list with four elements
brailleBar :: Int -> [Int] -> String
brailleBar meterlength = braille . map (unary meterlength)
  where
  braille [ (a1:a2 :as) -- 1   8
          , (b1:b2 :bs) -- 2  16
          , (c1:c2 :cs) -- 4  32
          , (d1:d2 :ds) --64 128
					] = (toEnum $ 10240 + (sum $ zipWith (*) [a1,b1,c1,d1,a2,b2,c2,d2] [1,2,4,64,8,16,32,128]) :: Char) : (braille [as,bs,cs,ds])
  braille [(a:[]), (b:[]), (c:[]), (d:[])] = [toEnum $ 10240 + (sum $ zipWith (*) [a,b,c,d] [1,2,4,64]) :: Char]
  braille _ = []

--max (k, n) ones padded to length k with zeroes
unary 0 _ = []
unary k 0 = replicate k 0
unary k n = 1 : unary (k-1) (n-1)


