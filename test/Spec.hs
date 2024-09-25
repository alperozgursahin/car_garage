{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, lib2Tests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - empty input" $
      Lib2.parseQuery "" @?= Left "Unknown command",
    testCase "Parsing case 2 - single character" $
      Lib2.parseQuery "o" @?= Left "Unknown command",
    testCase "Parsing case 3 - unknown command" $
      Lib2.parseQuery "unknown" @?= Left "Unknown command",
    testCase "Parsing case 4 - valid add command" $
      Lib2.parseQuery "add Toyota Camry 2020" @?= Right (Lib2.Add (Lib2.Car "Toyota" "Camry" 2020) Lib2.Quit)
  ]

lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 tests"
  [ testCase "Parse add command with one car" $
      Lib2.parseQuery "add Toyota Camry 2020" @?= Right (Lib2.Add (Lib2.Car "Toyota" "Camry" 2020) Lib2.Quit),

    testCase "Parse add command with multiple cars" $
      Lib2.parseQuery "add Toyota Camry 2020, Honda Civic 2021" @?= Right (Lib2.Add (Lib2.Car "Toyota" "Camry" 2020) (Lib2.Add (Lib2.Car "Honda" "Civic" 2021) Lib2.Quit)),

    testCase "Parse remove command" $
      Lib2.parseQuery "remove 1" @?= Right (Lib2.Remove 1),

    testCase "Parse list command" $
      Lib2.parseQuery "list" @?= Right Lib2.List,

    testCase "Parse quit command" $
      Lib2.parseQuery "quit" @?= Right Lib2.Quit,

    testCase "State transition add multiple cars" $
      Lib2.stateTransition Lib2.emptyState (Lib2.Add (Lib2.Car "Toyota" "Camry" 2020) (Lib2.Add (Lib2.Car "Honda" "Civic" 2021) Lib2.Quit)) @?= Right (Just "Cars added.", Lib2.State [Lib2.Car "Toyota" "Camry" 2020, Lib2.Car "Honda" "Civic" 2021]),

    testCase "State transition list cars after adding multiple" $
      Lib2.stateTransition (Lib2.State [Lib2.Car "Toyota" "Camry" 2020, Lib2.Car "Honda" "Civic" 2021]) Lib2.List @?= Right (Just "1: Car {make = \"Toyota\", model = \"Camry\", year = 2020}\n2: Car {make = \"Honda\", model = \"Civic\", year = 2021}", Lib2.State [Lib2.Car "Toyota" "Camry" 2020, Lib2.Car "Honda" "Civic" 2021]),

    testCase "State transition remove car" $
      Lib2.stateTransition (Lib2.State [Lib2.Car "Toyota" "Camry" 2020, Lib2.Car "Honda" "Civic" 2021]) (Lib2.Remove 1) @?= Right (Just "Car removed.", Lib2.State [Lib2.Car "Honda" "Civic" 2021]),

    testCase "Parse invalid remove command (non-existent ID)" $
      Lib2.stateTransition (Lib2.State [Lib2.Car "Toyota" "Camry" 2020]) (Lib2.Remove 99) @?= Right (Just "Invalid car ID.", Lib2.State [Lib2.Car "Toyota" "Camry" 2020]),

    testCase "State transition list cars with no cars" $
      Lib2.stateTransition Lib2.emptyState Lib2.List @?= Right (Just "No cars available.", Lib2.emptyState),

    testCase "Parse list command after adding cars" $
      Lib2.stateTransition (Lib2.State [Lib2.Car "Toyota" "Camry" 2020, Lib2.Car "Honda" "Civic" 2021]) Lib2.List @?= Right (Just "1: Car {make = \"Toyota\", model = \"Camry\", year = 2020}\n2: Car {make = \"Honda\", model = \"Civic\", year = 2021}", Lib2.State [Lib2.Car "Toyota" "Camry" 2020, Lib2.Car "Honda" "Civic" 2021]),

    testCase "State transition quit command" $
      Lib2.stateTransition Lib2.emptyState Lib2.Quit @?= Right (Just "Goodbye!", Lib2.emptyState)
  ]
