import Data.List

-- https://www.reddit.com/r/haskellquestions/comments/3akbtb/how_to_convert_string_to_a_list_of_integers_and/


main :: IO ()
main = interact $ unlines . lines