{-# LANGUAGE TupleSections #-}
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)

type Range = (Int, Int)             -- (start, length)
type Rule  = (Int, Int, Int)        -- (destStart, sourceStart, length)
type Map   = [Rule]

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

-- Parse the full input and retain map headers
parseInputWithLabels :: String -> ([Range], [(String, Map)])
parseInputWithLabels input =
  let ls = lines input
      (seedLine:restLines) = dropWhile null ls
      seedNums = map read $ tail $ words seedLine
      seedRanges = toRanges seedNums
      blocks = splitWhen null $ drop 1 restLines
      mapsWithLabels = map parseMapBlock blocks
  in (seedRanges, mapsWithLabels)

-- Turn [a1, b1, a2, b2, ...] into [(a1,b1), (a2,b2), ...]
toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (x:y:rest) = (x, y) : toRanges rest
toRanges _ = error "Uneven seed range list"

-- Parse a labeled map block
parseMapBlock :: [String] -> (String, Map)
parseMapBlock [] = error "Empty block"
parseMapBlock (header:ruleLines) =
  let label = header
      rules = mapMaybe parseRule ruleLines
  in (label, rules)

parseRule :: String -> Maybe Rule
parseRule line =
  case map read (words line) of
    [a, b, c] -> Just (a, b, c)
    _         -> Nothing

-- Split a list into sublists at blank lines
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
  let (pre, rest) = break p xs
      remaining = dropWhile p rest
  in pre : splitWhen p remaining

-- Apply all maps in sequence to all ranges
applyAllMaps :: [Map] -> [Range] -> [Range]
applyAllMaps maps ranges = foldl' (flip applyMapToRanges) ranges maps

-- Apply one map to all ranges
applyMapToRanges :: Map -> [Range] -> [Range]
applyMapToRanges rules = concatMap (applyMapToRange rules)

-- Apply one map to one range, using only the first matching rule
applyMapToRange :: Map -> Range -> [Range]
applyMapToRange rules range = applyRules rules range

-- Apply rules one-by-one; return result of first that modifies range
applyRules :: [Rule] -> Range -> [Range]
applyRules [] r = [r]
applyRules (rule:rs) r =
  let result = applyRule rule r
  in if result /= [r] then result else applyRules rs r

-- Apply a single rule to a range, possibly splitting it
applyRule :: Rule -> Range -> [Range]
applyRule (dest, src, len) (start, rlen) =
  let end     = start + rlen
      srcEnd  = src + len
      overStart = max start src
      overEnd   = min end srcEnd
  in if overStart >= overEnd then [(start, rlen)]  -- no overlap
     else
       let beforeLen = overStart - start
           afterLen  = end - overEnd
           before = if beforeLen > 0 then [(start, beforeLen)] else []
           mappedStart = dest + (overStart - src)
           mappedLen   = overEnd - overStart
           mapped = [(mappedStart, mappedLen)]
           after = if afterLen > 0 then [(overEnd, afterLen)] else []
       in before ++ mapped ++ after
