{-# LANGUAGE TupleSections #-}
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import System.IO (isEOF)

type Range = (Int, Int)             -- (start, length)
type Rule  = (Int, Int, Int)        -- (destStart, sourceStart, length)
type Map   = [Rule]

main :: IO ()
main = do
  input <- getContents
  let (seedRanges, mapsWithNames) = parseInputWithLabels input
  putStrLn "Seed ranges:"
  forM_ seedRanges $ \(start, len) ->
    putStrLn $ "  " ++ show start ++ " .. " ++ show (start + len - 1)

  putStrLn "\nParsed maps:"
  forM_ mapsWithNames $ \(label, rules) -> do
    putStrLn $ label ++ ":"
    forM_ rules $ \(dest, src, len) ->
      putStrLn $ "  " ++ show src ++ " .. " ++ show (src + len - 1)
             ++ " -> " ++ show dest ++ " .. " ++ show (dest + len - 1)

  let maps = map snd mapsWithNames
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

-- | Turn [a1, b1, a2, b2, ...] into [(a1,b1), (a2,b2), ...]
toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (x:y:rest) = (x, y) : toRanges rest
toRanges _ = error "Uneven seed range list"

-- | Parse a map block, extracting the label and its rules
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

-- | Split a list into chunks wherever the predicate is true
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
  let (pre, rest) = break p xs
      remaining = dropWhile p rest
  in pre : splitWhen p remaining

-- | Apply all maps in sequence to a list of seed ranges
applyAllMaps :: [Map] -> [Range] -> [Range]
applyAllMaps maps ranges = foldl' (\acc m -> applyMapToRanges m acc) ranges maps

-- | Apply one map to a list of ranges
applyMapToRanges :: Map -> [Range] -> [Range]
applyMapToRanges rules = concatMap (applyMapToRange rules)

-- | Apply a map to a single range
applyMapToRange :: Map -> Range -> [Range]
applyMapToRange [] r = [r]
applyMapToRange rules range = apply rules [range]

-- | Apply a list of rules to a set of ranges
apply :: [Rule] -> [Range] -> [Range]
apply [] rs = rs
apply (r:rs) ranges = apply rs (concatMap (applyRule r) ranges)

-- | Apply one rule to a range, possibly splitting it
applyRule :: Rule -> Range -> [Range]
applyRule (dest, src, len) (start, rlen) =
  let end     = start + rlen
      srcEnd  = src + len
      overStart = max start src
      overEnd   = min end srcEnd
  in if overStart >= overEnd then [(start, rlen)]
     else
       let beforeLen = overStart - start
           afterLen  = end - overEnd
           before = if beforeLen > 0 then [(start, beforeLen)] else []
           mappedStart = dest + (overStart - src)
           mappedLen = overEnd - overStart
           mapped = [(mappedStart, mappedLen)]
           after = if afterLen > 0 then [(overEnd, afterLen)] else []
       in before ++ mapped ++ after
