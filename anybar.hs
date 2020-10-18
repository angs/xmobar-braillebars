{- Copyright : Altti Tammi
 - License   : BSD3 (See LICENSE)
 -}

{-# LANGUAGE BangPatterns #-}
import System.IO
import BrailleBar
import Control.Monad (when)
import Data.Char
import System.Environment
import Common
import System.Console.GetOpt

data Options = Options 
  { minv :: Double
  , maxv :: Double
  , title :: String
  , meterChars :: Int
  } deriving Show

defaultOpts = Options
  { minv = 0.0
  , maxv = 100.0
  , title = ""
  , meterChars = 16
  }

meterDots         = (*2).meterChars

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['n'] ["min"]
     (ReqArg (\v opts -> opts { minv = read v }) "VALUE") $
     "lower boundary        (default: " ++ (show $ minv defaultOpts) ++ ")"
	, Option ['x'] ["max"]
	   (ReqArg (\v opts -> opts { maxv = read v }) "VALUE") $
		 "upper boundary        (default: " ++ (show $ maxv defaultOpts) ++ ")"
	, Option ['t'] ["title"]
	   (ReqArg (\s opts -> opts { title = s }) "STRING")
		 "title"
	, Option ['l'] ["length"]
	   (ReqArg (\v opts -> opts { meterChars = read v }) "VALUE") $
		 "Meter character count (default: " ++ (show $ meterChars defaultOpts) ++ ")"
  ]

compileOpts :: [String] -> IO (Options, [String])
compileOpts argv = 
  case getOpt RequireOrder options argv of
    (o, n, []) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo errHeader options))

errHeader = "Usage: anybar [OPTION...] value"

main = do
  a <- getArgs
  o@(opts,n) <- compileOpts a
  when (null n) $ ioError (userError ("Please provide a value\n" ++ usageInfo errHeader options))
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  let ((i,_):_) = reads $ n!!0
  let v = (i - minv opts) / (maxv opts) * (fromIntegral $ meterDots opts)
  let barString = intToBraille (meterDots opts)
                    [ 0
                    , floor v
                    , floor $ v + 0.5
                    , 0
                    ]
  let (lowstr, temp)  = splitAt (div (meterChars opts) 2) barString
  let (medstr, histr) = splitAt (div (meterChars opts) 4) temp
  mapM_ putStr 
    [ title opts, "\10288"
    , "<fc=", lowColor, ">", lowstr, "</fc>"
    , "<fc=", mediumColor, ">", medstr, "</fc>"
    , "<fc=", highColor, ">", histr, "</fc>"
    , "\10246\n"
    ]
  hFlush stdout
