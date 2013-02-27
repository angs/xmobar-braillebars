{- Copyright : Altti Tammi
 - License   : BSD3 (See LICENSE)
 -}

{-# LANGUAGE BangPatterns #-}
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.List (isPrefixOf)
import System.IO
import Data.IORef
import BrailleBar

meterchars = 16
meterdots = 2*meterchars

low="#13ad2f"
medium="#feb500"
high="#f5010a"
delay=1000000

main = do
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  stat <- newIORef ([[]] :: [[Int]])
  !oldstat <- cpuData
  writeIORef stat oldstat
  forever $ do
    threadDelay delay
    newstat <- cpuData
    oldstat <- readIORef stat
    let !load = zipWith coreLoad oldstat newstat
    writeIORef stat newstat
    let ll = length load
    let load' = map (numDots meterdots) $ take 4 $ load ++ (replicate (4 - ll) 0.0)
    let cpustring = brailleBar (meterdots) load'
    let (lowstr, temp) = splitAt (div meterchars 2) cpustring
    let (medstr, histr) = splitAt (div meterchars 4) temp
    putStr "CPU"
    putChar $ ['\10248','\10264','\10296','\10424']!!(ll-1)
    putStr $ concat
      [ "<fc=", low, ">", lowstr, "</fc>"
      , "<fc=", medium, ">", medstr, "</fc>"
      , "<fc=", high, ">", histr, "</fc>"
      ]
    putChar $ ['\10241','\10243','\10247','\10311']!!(ll-1)
    putChar '\n'
    hFlush stdout

cpuData :: IO [[Int]]
cpuData = do
  f <- readFile "/proc/stat"
  return $ map (map read . take 4 . tail . words) . takeWhile ("cpu" `isPrefixOf`) . tail . lines $ f

coreLoad :: (Integral a, Fractional b) => [a] -> [a] -> b
coreLoad (a1:a2:a3:a4:_) (b1:b2:b3:b4:_) = (fromIntegral k) / (fromIntegral $ k + b4 - a4)
  where k = b1+b2+b3-a1-a2-a3

numDots totaldots i = floor $ i*(fromIntegral $ totaldots)+0.5

