import System.Random
import Data.List

member :: (Eq a) => a -> [a] -> Bool
member a = any (==a) 

type AbyssRune = Char

runes = [['A'..'F'],['G'..'K'],['L'..'O']]

-- | finds how many slots a rune takes up.
runeSize :: AbyssRune -> Int
runeSize = fn runes where
  fn [] _ = error "invalid rune"
  fn (l:ls) a | any (==a) l = 1
  fn (l:ls) a = 1 + fn ls a

randomElem :: (RandomGen r, UniformRange a) => r -> [a] -> (a,r)
randomElem g l = let (i,ng) = uniformR (0,length l-1) g in (l !! i,ng)

randomRune :: RandomGen r => [AbyssRune] -> Int -> r -> (AbyssRune,r)
-- | gets a random abyss rune not found in e that uses at most m slots.
randomRune e m g = randomElem g $ (concat $ take m runes) \\ e

randomRunes :: RandomGen r => [AbyssRune] -> Int -> r -> ([AbyssRune],r)
randomRunes rs s g | s == 0 = (rs,g)
randomRunes rs s g | s < 0 = error "runes do not fit in slots"
randomRunes rs s g = let (r,ng) = (randomRune rs s g) in
  randomRunes (r:rs) (s - runeSize r) ng 

-- | a version of randomRunes using the standard RNG
randRunes :: Int -> IO [AbyssRune]
randRunes n = do
  g <- getStdGen
  return $ fst $ randomRunes [] n g

main = randRunes 8 >>= putStrLn . sort2
