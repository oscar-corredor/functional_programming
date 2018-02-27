-- process :: [Int] -> [Int] -> [Int]
-- process [] acc = acc
-- process (x:xs) acc = (process xs (acc++[(x+1)]))
import Data.List
gen f = xs where xs = map f $ inits xs
inflist f = ( f [] ) : map f (inits f)
