module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import qualified Data.List as L

type School = M.Map Grade [String]
type Grade = Int
type Name = String

add :: Grade -> Name -> School -> School
add gradeNum student school = M.insertWith (flip (++)) gradeNum [student] school

empty :: School
empty = M.empty

grade :: Grade -> School -> [Name]
grade gradeNum school =
  case (M.lookup gradeNum school) of
    Nothing  -> []
    Just val -> val
    
sorted :: School -> [(Grade, [Name])]
sorted school = map sorted $ M.toAscList school
  where
    sorted (grade, names) = (grade, L.sort names)
