module School (School, add, empty, grade, sorted) where

import Data.Bifunctor (second)
import Data.List (sort)
import Data.Map (Map, assocs, empty, findWithDefault, insertWith)

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = insertWith (<>) gradeNum [student]

grade :: Int -> School -> [String]
grade gradeNum = sort . findWithDefault mempty gradeNum

sorted :: School -> [(Int, [String])]
sorted = map (second sort) . assocs
