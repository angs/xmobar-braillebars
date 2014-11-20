{- Copyright : Altti Tammi
 - License   : BSD3 (See LICENSE)
 -}

{-# LANGUAGE BangPatterns #-}
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import System.IO
import Data.IORef
import BrailleBar
import System.Directory (doesDirectoryExist)
import System.Environment

charsPerMagnitude = 4
meterDots         = 3*2*charsPerMagnitude
lowColor          = "#13ad2f"
mediumColor       = "#feb500"
highColor         = "#f5010a"
milliSecond       = 1000
delay             = 1000*milliSecond
second            = 1000000.0
delayMultiplier   = second / (fromIntegral delay)

main = do
  a <- getArgs
  when (null a) $ error "Usage: netbar interface\ne.g. netbar eth0"
  let interface = head a
  interfaceExists <- doesDirectoryExist $ "/sys/class/net/" ++ interface
  when (not interfaceExists) $ error $ "Network interface not found: " ++ interface
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  stat <- newIORef ((0,0) :: (Integer, Integer))
  !oldstat <- netData interface
  writeIORef stat oldstat
  forever $ do
    threadDelay delay
    newstats@(rx_new, tx_new) <- netData interface
    (rx_old, tx_old)          <- readIORef stat
    writeIORef stat newstats
    let !rx = usageDots . (*delayMultiplier) . fromIntegral $ rx_new - rx_old
    let !tx = usageDots . (*delayMultiplier) . fromIntegral $ tx_new - tx_old
    let netstring       = brailleBar meterDots [0, rx, tx, 0]
    let (lowstr, temp)  = splitAt charsPerMagnitude netstring
    let (medstr, histr) = splitAt charsPerMagnitude temp
    mapM_ putStr 
      [ interface, " "
      , "<fc=#ffffff>B</fc>"
      , "<fc=", lowColor, ">", lowstr, "</fc>"
      , "<fc=#ffffff>K</fc>"
      , "<fc=", mediumColor, ">", medstr, "</fc>"
      , "<fc=#ffffff>M</fc>"
      , "<fc=", highColor, ">", histr, "</fc>"
      , "<fc=#ffffff>G</fc>\n"
      ]
    hFlush stdout

usageDots :: Double -> Int
usageDots n = floor $ log (n + 1) * (fromIntegral $ 2*charsPerMagnitude) / log 1024

netData :: String -> IO (Integer, Integer)
netData interface = do
  rx <- readFile $ "/sys/class/net/"++interface++"/statistics/rx_bytes"
  tx <- readFile $ "/sys/class/net/"++interface++"/statistics/tx_bytes"
  return (read rx, read tx)

