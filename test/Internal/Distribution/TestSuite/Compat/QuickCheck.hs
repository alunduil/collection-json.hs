{-|
Module       : Internal.Distribution.TestSuite.Compat.QuickCheck
Descpription : Compatability with QuickCheck
Copyright    : (c) Alex Brandt, 2017
License      : MIT

A compatability layer to integrate "QuickCheck" into Cabal's detailed test
running system.
-}
module Internal.Distribution.TestSuite.Compat.QuickCheck (qcTest) where

import Distribution.TestSuite (Progress (Finished), Result (Fail, Pass), TestInstance (TestInstance), Test (Test))
import Test.QuickCheck (quickCheckResult, Result (Failure, GaveUp, InsufficientCoverage, NoExpectedFailure, reason, Success), Testable)

import qualified Distribution.TestSuite as TS (Result)
import qualified Test.QuickCheck as QC (Result)

qcTest :: Testable p => String -> [String] -> p -> Test
qcTest n ts p = Test $ TestInstance (fromProperty p) n ts [] undefined

fromProperty :: Testable p => p -> IO Progress
fromProperty = fmap (Finished . fromResult) . quickCheckResult

fromResult :: QC.Result -> TS.Result
fromResult Success {}              = Pass
fromResult GaveUp {}               = Fail "QuickCheck Gave Up"
fromResult Failure {reason = r}    = Fail $ "QuickCheck Failed: " ++ r
fromResult NoExpectedFailure {}    = Fail "QuickCheck Expected Failure"
fromResult InsufficientCoverage {} = Fail "QuickCheck Passed with Insufficient Coverage"
