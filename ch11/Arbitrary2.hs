
import Arbitrary
import Test.QuickCheck (Gen)

instance Arbitrary Ternery where
  arbitrary = do
    n <- choose (0,2) :: Gen Int
    return $ case n of
      0 -> Yes
      1 -> No
      _ -> Unknown
      
