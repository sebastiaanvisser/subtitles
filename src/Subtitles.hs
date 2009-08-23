module Main where

import Control.Applicative
import Data.Maybe
import Data.Char
import Data.Ord
import Data.List
import Data.List.Split
import System.Environment

-- Subtitle data type.

data Time = Time
  { hours        :: Int
  , minutes      :: Int
  , seconds      :: Int
  , milliseconds :: Int
  } deriving (Eq, Ord)

data Range = Range 
  { from :: Time
  , to   :: Time
  } deriving (Eq, Ord)

data Line = Line
  { index :: Int
  , time  :: Range
  , subs  :: [String]
  } deriving (Eq, Ord)

type Subtitles = [Line]

-- Duration of a shift or prefix/suffix

data Duration = Duration
  { dMilliseconds :: Int
  , dMinutes      :: Int
  , dSeconds      :: Int
  , dHours        :: Int
  } deriving (Eq, Ord)

-- Showing subtilte lines.

instance Show Time where
  show (Time h m s l) = concat [f h, ":", f m, ":", f s, ",", g l]
    where
      f x | x < 10    = "0" ++ show x
          | otherwise = show x
      g x | x < 10    = "00" ++ show x
          | x < 100   = "0" ++ show x
          | otherwise = show x

instance Show Range where
  show (Range f t) = concat [show f, " --> ", show t]

instance Show Line where
  show (Line i t s) =
    intercalate "\n" (show i : show t : s) ++ "\n"

-- SRT format parser.

parse :: String -> Subtitles
parse =
    catMaybes
  . map parseSingle
  . map lines
  . splitOn "\n\n"
  . unlines
  . map trim    
  . lines

parseSingle :: [String] -> Maybe Line
parseSingle (i:t:xs) = (flip (Line (read i)) xs) <$> parseTimes t
parseSingle _        = Nothing

parseTimes :: String -> Maybe Range
parseTimes t =
  case splitOn "-->" t of
    [a, b] -> Range <$> parseTime a <*> parseTime b
    _      -> Nothing

parseTime :: String -> Maybe Time
parseTime t =
  let xs =  fmap read . splitWhen (not . isDigit) $ trim t in
  case xs of
    [h, m, s, l] -> Just (Time h m s l)
    _            -> Nothing

-- Shifting time ranges.

shift :: Duration -> Subtitles -> Subtitles
shift d = map (shiftLine d)

shiftLine :: Duration -> Line -> Line
shiftLine d (Line i t w) = Line i (shiftRange d t) w

shiftRange :: Duration -> Range -> Range
shiftRange d (Range f t) = Range (shiftTime d f) (shiftTime d t)

shiftTime :: Duration -> Time -> Time
shiftTime (Duration dl ds dm dh) (Time th tm ts tl) = 
  let li = (tl + dl)
      se = (ts + ds + li `div` 1000)
      mi = (tm + dm + se `div` 60)
      ho = (th + dh + mi `div` 60)
  in Time ho (mi `mod` 60) (se `mod` 60) (li `mod` 1000)

reindex :: Subtitles -> Subtitles
reindex s = zipWith f s [1..]
  where f (Line _ t w) i = Line i t w

prefix :: Duration -> Subtitles -> Subtitles
prefix (Duration l s m h) =
  filter ((== GT) . compare (Time h m s l) . (from . time))

suffix :: Duration -> Subtitles -> Subtitles
suffix (Duration l s m h) =
    shift (Duration (-l) (-s) (-m) (-h))
  . reindex
  . filter ((/= GT) . compare (Time h m s l) . (from . time))

-- Helper functions.

withReverse :: ([a] -> [a]) -> [a] -> [a]
withReverse f = reverse . f . reverse

withBoth :: ([a] -> [a]) -> [a] -> [a]
withBoth f = withReverse f . f

trim :: String -> String
trim = withBoth (dropWhile isSpace)

-- Main function.

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["show",    srt]          -> with id               srt
    ("shift" :  srt : x : xs) -> with (shift' (x:xs))  srt
    ["reindex", srt]          -> with reindex          srt
    ("prefix" : srt : x : xs) -> with (prefix' (x:xs)) srt
    ("suffix" : srt : x : xs) -> with (suffix' (x:xs)) srt
    _                         -> do
      putStrLn "USAGE:"
      putStrLn "subtitles show    <srt>"
      putStrLn "subtitles shift   <srt> <milliseconds> [<seconds> [<minutes> [<hours>]]]"
      putStrLn "subtitles reindex <srt>"
      putStrLn "subtitles prefix  <srt> <seconds> [<minutes> [<hours>]]"
      putStrLn "subtitles suffix  <srt> <seconds> [<minutes> [<hours>]]"
  where
    with f srt = readFile srt >>= mapM_ print . f . parse

    shift' [l]          = shift (Duration (read l) 0        0        0)
    shift' [l, s]       = shift (Duration (read l) (read s) 0        0)
    shift' [l, s, m]    = shift (Duration (read l) (read s) (read m) 0)
    shift' [l, s, m, h] = shift (Duration (read l) (read s) (read m) (read h))
    shift' _            = error "shift: too many arguments"

    prefix' [s]          = prefix (Duration 0 (read s) 0        0)
    prefix' [s, m]       = prefix (Duration 0 (read s) (read m) 0)
    prefix' [s, m, h]    = prefix (Duration 0 (read s) (read m) (read h))
    prefix' _            = error "prefix: too many arguments"

    suffix' [s]          = suffix (Duration 0 (read s) 0        0)
    suffix' [s, m]       = suffix (Duration 0 (read s) (read m) 0)
    suffix' [s, m, h]    = suffix (Duration 0 (read s) (read m) (read h))
    suffix' _            = error "suffix: too many arguments"

