{- Copyright : Altti Tammi
 - License   : BSD3 (See LICENSE)
 -}

{-# LANGUAGE BangPatterns #-}
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import System.IO
import Common
import Data.IORef
import BrailleBar
import System.Directory (doesDirectoryExist)
import System.Environment
import qualified Data.Sequence as S

charsPerMagnitude = 4

divByWindowSeconds= second / (fromIntegral $ windowSize * delay)
meterDots         = 3*2*charsPerMagnitude

main = do
  a <- getArgs
  when (null a) $ error "Usage: netbar interface\ne.g. netbar eth0"
  let interface = head a
  interfaceExists <- doesDirectoryExist $ "/sys/class/net/" ++ interface
  when (not interfaceExists) $ error $ "Network interface not found: " ++ interface
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  stat <- newIORef $ S.replicate windowSize ((0,0) :: (Integer, Integer))
  forever $ do
    threadDelay delay
    newstats@(rx_new, tx_new) <- netData interface
    (rx_old, tx_old)          <- (`S.index` 0) <$> readIORef stat
    modifyIORef' stat (S.drop 1 . (S.|> newstats))
    let !rx = usageDots . (*divByWindowSeconds) . fromIntegral $ rx_new - rx_old
    let !tx = usageDots . (*divByWindowSeconds) . fromIntegral $ tx_new - tx_old
    let netstring       = intToBraille meterDots [0, rx, tx, 0]
    let (lowstr, temp)  = splitAt charsPerMagnitude netstring
    let (medstr, histr) = splitAt charsPerMagnitude temp
    mapM_ putStr 
      [ interface, " "
      , "<fc=#ffffff>B</fc>"
      , "<fc=", lowColor, ">"
      , lowstr
      , "</fc>", "<fc=#ffffff>K</fc>"
      , "<fc=", mediumColor, ">"
      , medstr
      , "</fc>", "<fc=#ffffff>M</fc>"
      , "<fc=", highColor, ">"
      , histr
      , "</fc>", "<fc=#ffffff>G</fc>\n"
      ]
    hFlush stdout

usageDots :: Double -> Int
usageDots n = round $ log (n + 1) * (fromIntegral $ 2*charsPerMagnitude) / log 1000

netData :: String -> IO (Integer, Integer)
netData interface = do
  rx <- readFile $ "/sys/class/net/"++interface++"/statistics/rx_bytes"
  tx <- readFile $ "/sys/class/net/"++interface++"/statistics/tx_bytes"
  return (read rx * 8, read tx * 8)

