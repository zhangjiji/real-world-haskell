{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module BrokenClass where

import JSONClass (JSON, toJValue, fromJValue)
import JSONClassExport (JAry)

-- instance (JSON a) => JSON [a] where
--   toJValue = undefined
--   fromJValue = undefined

-- instance (JSON a) => JSON [(String, a)] where
--   toJValue = undefined
--   fromJValue = undefined

newtype Test = Test {
  getTest :: Int
  } deriving (Show)
