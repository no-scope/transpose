import System.IO
import System.Environment
import System.Directory
import Data.Maybe
import Data.List hiding (transpose)

sharpKeys = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]

flatKeys  = ["A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"]

parseKeys = ["A#", "C#", "D#", "F#", "G#", "Bb", "Db", "Eb", "Gb", "Ab", "A",
             "B", "C", "D", "E", "F", "G"]

-- Transpose the chord
move :: Int -> String -> String
move i s = sharpKeys !! ((j + i) `mod` 12)
  where Just j = elemIndex s sharpKeys


nextChord :: String -> Maybe String
nextChord s = find (\x -> x `isPrefixOf` s) parseKeys

-- Parse the song and change the chords
transpose :: String -> Int -> String
transpose s i = go s
  where
    go [] = ""
    go xs
      | isNothing tmp   = head xs : (go $ tail xs)
      | oldLen < newLen = newChord ++ (go $ remSpace rem)
      | otherwise       = newChord ++ go rem
        where
          tmp      = nextChord xs
          oldChord = fromJust tmp
          oldLen   = length oldChord
          newChord = move i oldChord
          newLen   = length newChord
          rem      = drop oldLen xs

remSpace :: String -> String
remSpace xs =
  case xs of
    []         ->  []
    ('\n' : ys)  ->  '\n' : ys
    (' '  : ys)  ->  ys
    ( y   : ys)  ->  y : remSpace ys

main = do
  [filename, n] <- getArgs
  handle        <- openFile filename ReadMode
  song          <- hGetContents handle
  putStr $ transpose song (read n :: Int)
  hClose handle
