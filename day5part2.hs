{-# LANGUAGE TupleSections #-}
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)

type Range = (Int, Int)             -- (start, length)
type Rule = (Int, Int, Int)         -- (destStart, sourceStart, length)
type Map = [Rule]

main :: IO ()
main = do
  input <- getContents
  let (seedRanges, mapsWithLabels) = parseInputWithLabels input
  putStrLn "Seed ranges:"
  forM_ seedRanges $ \(s, l) ->
    putStrLn $ "  " ++ show s ++ " .. " ++ show (s + l - 1)

  putStrLn "\nParsed maps:"
  forM_ mapsWithLabels $ \(label, rules) -> do
    putStrLn $ label ++ ":"
    forM_ rules $ \(dest, src, len) ->
      putStrLn $ "  " ++ show src ++ " .. " ++ show (src + len - 1)
             ++ " -> " ++ show dest ++ " .. " ++ show (dest + len - 1)

  let maps = map snd mapsWithLabels
      finalRanges = applyAllMaps maps seedRanges
      lowest = minimum $ map fst finalRanges
  putStrLn $ "\nLowest location: " ++ show lowest

-- | Parse the full input and retain map headers
parseInputWithLabels :: String -> ([Range], [(String, Map)])
parseInputWithLabels input =
  let ls = lines input
      (seedLine:restLines) = dropWhile null ls
      seedNums = map read $ tail $ words seedLine
      seedRanges = toRanges seedNums
      blocks = splitWhen null $ drop 1 restLines
      mapsWithLabels = map parseMapBlock blocks
  in (seedRanges, mapsWithLabels)

-- | Convert [a1, b1, a2, b2, ...] into [(a1, b1), (a2, b2), ...]
toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (a:b:rest) = (a, b) : toRanges rest
toRanges _ = error "Odd number of seed values; expected pairs."

-- | Parse a block of text into a named map
parseMapBlock :: [String] -> (String, Map)
parseMapBlock [] = error "Empty map block"
parseMapBlock (header:lines) =
  let label = header
      rules = mapMaybe parseRule lines
  in (label, rules)

parseRule :: String -> Maybe Rule
parseRule line =
  case map read (words line) of
    [a, b, c] -> Just (a, b, c)
    _         -> Nothing

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
  let (pre, rest) = break p xs
      remaining = dropWhile p rest
  in pre : splitWhen p remaining

-- Apply all maps to all ranges
applyAllMaps :: [Map] -> [Range] -> [Range]
applyAllMaps maps ranges = foldl' (flip applyMapToRanges) ranges maps

-- Apply one map to many ranges
applyMapToRanges :: Map -> [Range] -> [Range]
applyMapToRanges rules = concatMap (applyMapToRange rules)

-- Apply one map to a single range
applyMapToRange :: Map -> Range -> [Range]
applyMapToRange [] r = [r]
applyMapToRange rules r = apply rules [r]

-- Apply a list of rules to a list of ranges
apply :: [Rule] -> [Range] -> [Range]
apply [] rs = rs
apply (rule:rs) ranges = apply rs (concatMap (applyRule rule) ranges)

-- Apply one rule to a range (may split into 1–3 parts)
applyRule :: Rule -> Range -> [Range]
applyRule (dest, src, len) (start, rlen) =
  let end     = start + rlen
      srcEnd  = src + len
      overlapStart = max start src
      overlapEnd   = min end srcEnd
  in if overlapStart >= overlapEnd then [(start, rlen)] -- no overlap
     else
       let beforeLen = overlapStart - start
           afterLen  = end - overlapEnd
           before = if beforeLen > 0 then [(start, beforeLen)] else []
           mappedStart = dest + (overlapStart - src)
           mappedLen = overlapEnd - overlapStart
           mapped = [(mappedStart, mappedLen)]
           after = if afterLen > 0 then [(overlapEnd, afterLen)] else []
       in before ++ mapped ++ after
