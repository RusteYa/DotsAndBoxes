module LogicTest where

import           Game.Logic
import           Test.Tasty
import           Test.Tasty.HUnit

testIsAvailable =
  testCase "correct is available testing" $
  assertBool [] $
  isAvailable
    (newEdge (newPoint (0, 0), newPoint (0, 1)))
    [ newEdge (newPoint (0, 0), newPoint (0, 1))
    , newEdge (newPoint (0, 1), newPoint (1, 1))
    , newEdge (newPoint (1, 0), newPoint (2, 0))
    ]

testIsNotAvailable =
  testCase "correct not is available testing" $
  assertBool [] $
  isAvailable
    (newEdge (newPoint (1, 1), newPoint (2, 1)))
    [ newEdge (newPoint (0, 0), newPoint (0, 1))
    , newEdge (newPoint (0, 1), newPoint (1, 1))
    , newEdge (newPoint (1, 0), newPoint (2, 0))
    ]

isAvailableTests = testGroup "is available tests" [testIsAvailable, testIsNotAvailable]

testIsValid = testCase "correct is valid testing" $ assertBool [] $ isValid (newEdge (newPoint (0, 0), newPoint (0, 1)))

testIsNotValid =
  testCase "correct not is valid testing" $ assertBool [] $ isValid (newEdge (newPoint (0, 0), newPoint (1, 1)))

isValidTests = testGroup "isValid tests" [testIsValid, testIsNotValid]

testNormalizeEdgeV =
  testCase "correct normalizeEdge v testing" $
  assertEqual [] (newEdge (newPoint (0, 0), newPoint (1, 0))) $ normalizeEdge ["0", "0"] "h"

testNormalizeEdgeH =
  testCase "correct normalizeEdge h testing" $
  assertEqual [] (newEdge (newPoint (0, 0), newPoint (0, 1))) $ normalizeEdge ["0", "0"] "v"

testNormalizeEdgeOver =
  testCase "correct normalizeEdge over testing" $
  assertEqual [] (newEdge (newPoint (0, 0), newPoint (1, 0))) $ normalizeEdge ["0", "0"] "h"

normalizeEdgeTests = testGroup "normalizeEdge tests" [testNormalizeEdgeV, testNormalizeEdgeH, testNormalizeEdgeOver]

testChangePlayerFirstToSecond =
  testCase "correct change first player to second testing" $ assertEqual [] 2 $ changePlayer 1

testChangePlayerSecondToFirst =
  testCase "correct change second player to first testing" $ assertEqual [] 1 $ changePlayer 2

changePlayerTests = testGroup "change player tests" [testChangePlayerFirstToSecond, testChangePlayerSecondToFirst]

testDeleteAvailable =
  testCase "correct delete available testing" $
  assertEqual [] [newEdge (newPoint (0, 1), newPoint (1, 1)), newEdge (newPoint (1, 0), newPoint (2, 0))] $
  deleteAvailable
    (newEdge (newPoint (0, 0), newPoint (0, 1)))
    [ newEdge (newPoint (0, 0), newPoint (0, 1))
    , newEdge (newPoint (0, 1), newPoint (1, 1))
    , newEdge (newPoint (1, 0), newPoint (2, 0))
    ]

deleteAvailableTests = testGroup "delete available tests" [testDeleteAvailable]

logicTests =
  testGroup "logic tests" [isAvailableTests, isValidTests, normalizeEdgeTests, changePlayerTests, deleteAvailableTests]
