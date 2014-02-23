import Supply
import System.Random hiding (next)

randomIO :: Random a => IO [a]
randomIO =
  getStdRandom $ \g ->
  let (a, b) = split g
  in (randoms a, b)
     
