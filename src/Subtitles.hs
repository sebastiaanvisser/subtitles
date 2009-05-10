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
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , frame   :: Int
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

-- Showing subtilte lines.

instance Show Time where
  show (Time h m s f) = concat [show h, ":", show m, ":", show s, ",", show f]

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
    [a, b, c, d] -> Just (Time a b c d)
    _            -> Nothing

-- Shifting time ranges.

shift :: Int -> Int -> Int -> Subtitles -> Subtitles
shift h m s = map (shiftLine h m s)

shiftLine :: Int -> Int -> Int -> Line -> Line
shiftLine h m s (Line i t w) = Line i (shiftRange h m s t) w

shiftRange :: Int -> Int -> Int -> Range -> Range
shiftRange h m s (Range f t) = Range (shiftTime h m s f) (shiftTime h m s t)

shiftTime :: Int -> Int -> Int -> Time -> Time
shiftTime h m s (Time th tm ts tf) = 
  let se = (ts + s)
      mi = (tm + m + se `div` 60)
      ho = (th + h + mi `div` 60)
  in Time ho (mi `mod` 60) (se `mod` 60) tf

reindex :: Subtitles -> Subtitles
reindex s = zipWith f s [1..]
  where f (Line _ t w) i = Line i t w

prefix :: Int -> Int -> Int -> Subtitles -> Subtitles
prefix h m s =
  filter ((== GT) . compare (Time h m s 0) . (from . time))

suffix :: Int -> Int -> Int -> Subtitles -> Subtitles
suffix h m s =
    shift (-h) (-m) (-s)
  . reindex
  . filter ((/= GT) . compare (Time h m s 0) . (from . time))

-- Helper functions.

withReverse :: ([a] -> [a]) -> [a] -> [a]
withReverse f = reverse . f . reverse

withBoth :: ([a] -> [a]) -> [a] -> [a]
withBoth f = withReverse f . f

trim :: String -> String
trim = withBoth (dropWhile isSpace)

-- Main function.

main :: IO ()
main =
  do args <- getArgs
     case args of

       ["show",    srt]          -> with id                                  srt
       ["shift",   srt, h, m, s] -> with (shift (read h) (read m) (read s))  srt
       ["reindex", srt]          -> with reindex                             srt
       ["prefix",  srt, h, m, s] -> with (prefix (read h) (read m) (read s)) srt
       ["suffix",  srt, h, m, s] -> with (suffix (read h) (read m) (read s)) srt

       _ -> 
         do putStrLn "ERROR invalid arguments. Try one of the following:"
            putStrLn "  subtitles show <srt>"
            putStrLn "  subtitles shift <srt> <hour> <minutes> <seconds>"
            putStrLn "  subtitles reindex <srt>"
            putStrLn "  subtitles prefix <srt> <hour> <minutes> <seconds>"
            putStrLn "  subtitles suffix <srt> <hour> <minutes> <seconds>"

  where with f srt = readFile srt >>= mapM_ print . f . parse

