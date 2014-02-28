import System.Environment
import Text.Printf

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean [1..d])

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- ghc -O2 -rtsopts --make A.hs -prof -auto-all -caf-all -fforce-recomp
-- time ./A 1e6 +RTS -hc -p -K100M
-- hp2ps -e8in -c A.hp
