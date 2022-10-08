module Tutorial2 where 
import Data.List
import Data.Char

run :: String -> [(Char, Int)]
run = display . group . sort . canonical

canonical :: String -> String
canonical = filter  (/= ' ') . map normalise

normalise c | isUpper c = c
            | isLower c = toUpper c
            | otherwise  = ' '

display = map (\x -> (head x, length x))   
test2 = run "Hello World"