{-# LANGUAGE TupleSections #-}
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import System.IO (isEOF)

type Rule = (Int, Int, Int)         -- (destStart, sourceStart, length)
type Map  = [Rule]

main :: IO ()
main = do
  input <- getContents
  let (seeds, mapsWithNames) = parseInputWithLabels input
  putStrLn "Seeds:"
  forM_ seeds $ \s -> putStrLn $ "  " ++ show s

  putStrLn "\nParsed maps:"
  forM_ mapsWithNames $ \(label, rules) -> do
    putStrLn $ label ++ ":"
    forM_ rules $ \(dest, src, len) ->
      putStrLn $ "  " ++ show src ++ " .. " ++ show (src + len - 1)
             ++ " -> " ++ show dest ++ " .. " ++ show (dest + len - 1)

  let maps = map snd mapsWithNames
      finalValues = map (applyAllMaps maps) seeds
      lowest = minimum finalValues
  putStrLn $ "\nLowest location: " ++ show lowest

-- Parse input with labeled maps
parseInputWithLabels :: String -> ([Int], [(String, Map)])
parseInputWithLabels input =
  let ls = lines input
      (seedLine:restLines) = dropWhile null ls
      seedNums = map read $ tail $ words seedLine
      blocks = splitWhen null $ drop 1 restLines
      mapsWithLabels = map parseMapBlock blocks
  in (seedNums, mapsWithLabels)

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

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs =
  let (pre, rest) = break p xs
      remaining = dropWhile p rest
  in pre : splitWhen p remaining

-- Part One logic
applyAllMaps :: [Map] -> Int -> Int
applyAllMaps maps seed = foldl (\acc m -> applyMap m acc) seed maps

applyMap :: Map -> Int -> Int
applyMap [] n = n
applyMap ((dest, src, len):rs) n
  | n >= src && n < src + len = dest + (n - src)
  | otherwise = applyMap rs n
