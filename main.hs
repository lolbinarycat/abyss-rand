import System.Random
import Data.List

member :: (Eq a) => a -> [a] -> Bool
member a = any (==a) 

type AbyssRune = Char

runes = [['A'..'F'],['G'..'K'],['L'..'O']]

runeSize a | member a (runes !! 0) = 1
runeSize a | member a (runes !! 1) = 2
runeSize a | member a (runes !! 2) = 3

randomElem :: (RandomGen r, UniformRange a) => r -> [a] -> (a,r) 
randomElem g l = let (i,ng) = uniformR (0,length l) g in (l !! i,ng)

randomRune :: RandomGen r => [AbyssRune] -> Int -> r -> (AbyssRune,r)
-- | gets a random abyss rune not found in e that uses at most m slots.
randomRune e m g = randomElem g $ (concat $ take m runes) \\ e

randomRunes :: RandomGen r => [AbyssRune] -> Int -> r -> ([AbyssRune],r)
randomRunes rs s g | s == 0 = (rs,g)
randomRunes rs s g | s < 0 = error "runes do not fit in slots"
randomRunes rs s g = let (r,ng) = (randomRune rs s g) in
  randomRunes (r:rs) (s - runeSize r) ng 

main = getStdGen >>= (\g -> print $ fst $ randomRunes [] 8 g)
