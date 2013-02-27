{- Copyright : Altti Tammi
 - License   : BSD3 (See LICENSE)
 -}

{-# LANGUAGE BangPatterns #-}
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import System.IO
import Data.IORef
import BrailleBar
import System.Environment

charsPerMagnitude = 4
meterlength = 3*2*charsPerMagnitude

low="#13ad2f"
medium="#feb500"
high="#f5010a"
delay=1000000

main = do
  a <- getArgs
  when (null a) $ error "Usage: netbar interface\ne.g. netbar eth0"
  let interface = head a
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  stat <- newIORef ((0,0) :: (Integer, Integer))
  !oldstat <- netData interface
  writeIORef stat oldstat
  forever $ do
    threadDelay delay
    newstat <- netData interface
    oldstat <- readIORef stat
    writeIORef stat newstat
    let !load = (usageDots $ fst newstat - fst oldstat, usageDots $ snd newstat - snd oldstat)
    let netstring = brailleBar meterlength [0,fst load,snd load,0]
    let (lowstr, temp) = splitAt charsPerMagnitude netstring
    let (medstr, histr) = splitAt charsPerMagnitude temp
    putStrLn $ concat 
      [ interface ++ " "
      , "<fc=#ffffff>B</fc>"
      , "<fc=", low, ">", lowstr, "</fc>"
      , "<fc=#ffffff>K</fc>"
      , "<fc=", medium, ">", medstr, "</fc>"
      , "<fc=#ffffff>M</fc>"
      , "<fc=", high, ">", histr, "</fc>"
      , "<fc=#ffffff>G</fc>"
      ]
    hFlush stdout

usageDots :: Integer -> Int
usageDots n = floor $ log (fromIntegral $ max 1 n) * (fromIntegral $ 2*charsPerMagnitude) / log 1024

netData :: String -> IO (Integer, Integer)
netData interface = do
  rx <- readFile $ "/sys/class/net/"++interface++"/statistics/rx_bytes"
  tx <- readFile $ "/sys/class/net/"++interface++"/statistics/tx_bytes"
  return (read rx, read tx)

