import ControlledVisit (Info, getUsefulContents, getInfo)
import System.FilePath ((</>))
import System.Directory (isDirectory)

data Iterate seed = Done { unwrap :: seed}
                  | Skip { unwrap :: seed}
                  | Continue { unwrap :: seed}
                  deriving (Show)

type Interate seed = seed -> Info -> Iterate seed

foldTree :: Iterate a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed' -> walk seed' names
        Continue seed'
          | isDirectory info -> do
            next <- fold seed' path'
            case next of
              done@(Done _) -> return done
              seed'' -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)
