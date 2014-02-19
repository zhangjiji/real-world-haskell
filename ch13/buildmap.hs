import qualified Data.Map as Map

al = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]

mapFromAL = Map.fromList al

mapFold = foldl (\map (k,v) -> Map.insert k v map) Map.empty al

mapManual =
  Map.insert 2 "b" .
  Map.insert 4 "d" .
  Map.insert 1 "a" .
  Map.insert 3 "c" $ Map.empty
