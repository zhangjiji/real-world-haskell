{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module BrokenClass where

import JSONClass (JSON, toJValue, fromJValue)

instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined
