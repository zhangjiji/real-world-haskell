module Arbitrary where

import Test.QuickCheck (Gen)
import System.Random (Random)

class Arbitrary a where
  arbitrary :: Gen a
  elements :: [a] -> Gen a
  choose :: Random a => (a, a) -> Gen a
  oneof :: [Gen a] -> Gen a

data Ternery = Yes
             | No
             | Unknown
             deriving (Eq, Show)

instance Arbitrary Ternery where
  arbitrary = elements [Yes, No, Unknown]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (x, y)
