import Prettify2
import Test.QuickCheck.Batch

options = TestOptions {
  no_of_tests = 200
  , length_of_test = 1
  , debug_test = False }

main = do
  runTests "simple" options
  [ run prop_text
    , run prop_double]

  runTests "complex" options
  [ run prop_hcat
    ]
