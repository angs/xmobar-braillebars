{-# LANGUAGE BangPatterns #-}
import Control.Monad (liftM, forever)
import Control.Concurrent (threadDelay)
import Data.List (isPrefixOf)
import System.IO
import Data.IORef

meterlength = 16

low="#13ad2f"
medium="#feb500"
high="#f5010a"
delay=1000000

main = do
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  stat <- newIORef ([[]] :: [[Double]])
  !oldstat <- cpuData
  writeIORef stat oldstat
  forever $ do
    threadDelay delay
    newstat <- cpuData
    oldstat <- readIORef stat
    let !load = zipWith coreLoad oldstat newstat
    writeIORef stat newstat
    let ll = length load
    let load' = take 4 $ load ++ (replicate (4 - ll) 0.0)
    let cpus = cpuBar meterlength load'
    let (lowstr, temp) = splitAt (div meterlength 2) cpus
    let (medstr, histr) = splitAt (div meterlength 4) temp
    putStr "CPU"
    putChar $ ['\10248','\10264','\10296','\10424']!!(ll-1)
    putStr $ "<fc=" ++ low ++ ">" ++ lowstr ++ "</fc><fc=" ++ medium ++ ">" ++ medstr ++ "</fc><fc=" ++ high ++ ">" ++ histr ++ "</fc>"
    putChar $ ['\10241','\10243','\10247','\10311']!!(ll-1)
    putChar '\n'
    hFlush stdout

cpuData :: IO [[Double]]
cpuData = do
  f <- readFile "/proc/stat"
  return $ map (map read.tail.words) . takeWhile ("cpu" `isPrefixOf`) . tail . lines $ f

coreLoad :: Fractional a => [a] -> [a] -> a
coreLoad (a1:a2:a3:a4:_) (b1:b2:b3:b4:_) = k / (k + b4 - a4)
  where k = b1+b2+b3-a1-a2-a3

cpuBar :: Int -> [Double] -> String
cpuBar meterlength = br . map (unary totaldots) . map numdots
  where
  totaldots = 2*meterlength
  numdots i = floor $ i*(fromIntegral $ totaldots)+0.5
  br ([]:_) = []
  br [(a1:a2:as),(b1:b2:bs),(c1:c2:cs),(d1:d2:ds)] = (toEnum $ 10240+a1+2*b1+4*c1+8*a2+16*b2+32*c2+64*d1+128*d2 :: Char) : (br [as,bs,cs,ds])

unary 0 _ = []
unary k 0 = replicate k 0
unary k n = 1 : unary (k-1) (n-1)

