{- Copyright : Altti Tammi
 - License   : BSD3 (See LICENSE)
 -}

{-# LANGUAGE BangPatterns #-}
import Control.Monad (forever, forM_)
import Control.Concurrent (threadDelay)
import Data.List (isPrefixOf)
import Common
import System.IO
import qualified Data.Sequence as S
import Data.IORef
import BrailleBar
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromJust)
import qualified Data.Word8 as W

meterChars   = 16

meterDots    = 2*meterChars
lowLength    = div meterChars 2
mediumLength = div meterChars 4

main = do
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  numCPUs <- cpuCount
  stat <- newIORef $ S.replicate windowSize (replicate numCPUs [0,0,0,0])
  let numBars    = div (numCPUs + 3) 4
  let delimiters = map (\a -> [a]) $ if numCPUs <= 4
      then ["\10248\10241","\10264\10243","\10296\10247","\10424\10311"]!!(numCPUs - 1) 
      else '\10424' 
           : (replicate (max 0 (numBars - 2)) '\10495') 
           ++ (["\10495\10311","\10319\10241","\10335\10243","\10367\10247"]!!(mod numCPUs 4))
  forever $ do
    threadDelay delay
    newstats <- cpuData' numCPUs
    oldstats <- (`S.index` 0) <$> readIORef stat
    let !load = zipWith coreLoad oldstats newstats
    modifyIORef' stat (S.drop 1 . (S.|> newstats))
    let bars = map (intToBraille meterDots) $ chunksOf4 $ map (numDots meterDots) $ load
    let coloredBars = flip map bars $ (\bar -> 
                        let (lowstr, temp) = splitAt lowLength bar in
                        let (medstr, histr) = splitAt mediumLength temp in concat 
                        [ "<fc=", lowColor, ">", lowstr, "</fc>"
                        , "<fc=", mediumColor, ">", medstr, "</fc>"
                        , "<fc=", highColor, ">", histr, "</fc>"
                        ])
    putStr "CPU"
    mapM_ putStr $ interleave delimiters coloredBars
    putChar '\n'
    hFlush stdout
  where
  cpuData' :: Int -> IO [[Int]]
  cpuData' n = map (map (fst . fromJust . C8.readInt) . take 4 . tail . B.splitWith (==W._space)) . take n . tail . B.splitWith (==W._lf) <$> B.readFile "/proc/stat"

cpuCount :: IO Int
cpuCount = do
  f <- readFile "/proc/stat"
  return $ length . takeWhile ("cpu" `isPrefixOf`) . tail . lines $ f

coreLoad :: (Integral a, Fractional b) => [a] -> [a] -> b
coreLoad (a_user:a_nice:a_system:a_idle:_) (b_user:b_nice:b_system:b_idle:_) = (fromIntegral busy) / (fromIntegral $ busy + b_idle - a_idle)
  where busy = b_user + b_nice + b_system - a_user - a_nice - a_system

numDots :: Int -> Double -> Int
numDots totaldots i = round $ i*(fromIntegral $ totaldots)

chunksOf4 :: [Int] -> [[Int]]
chunksOf4 (a:b:c:d:ls) = [a,b,c,d] : chunksOf4 ls
chunksOf4 [] = []
chunksOf4 ls = [ls ++ (replicate (4 - (length ls)) 0)]

interleave [] _ = []
interleave (a:as) b = a : interleave b as
