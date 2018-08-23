module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Loops as M

import Data.Either (isLeft)
import Data.List as L
import Data.Unfoldable (replicate)

import Test.Assert (assert')

main :: Effect Unit
main = do
  test_whileM
  test_untilM
  test_andM
  test_orM
  test_anyPM
  test_allPM
  test_anyM
  test_allM
  issue1

  where
  decrementToZero n = do
    ref <- Ref.new n
    pure do
      n' <- Ref.read ref
      if n' <= 0
        then pure false
        else do
          Ref.modify_ (_ - 1) ref
          pure true

  trueList :: L.List Boolean
  trueList = replicate 10 true

  falseList :: L.List Boolean
  falseList = replicate 10 false

  altList :: L.List Boolean
  altList = join $ replicate 5 $ true L.: false L.: L.Nil

  test_whileM = do
    f <- decrementToZero 10
    a <- M.whileM f (pure 1)
    assert' "whileM" $ a == replicate 10 1

  test_untilM = do
    f <- decrementToZero 10
    a <- M.untilM (pure 1) (not <$> f)
    assert' "untilM" $ a == replicate 11 1

  test_andM = do
    a <- M.andM $ pure <$> trueList
    b <- M.andM $ pure <$> altList
    assert' "andM true" a
    assert' "andM alt" $ not b

  test_orM = do
    a <- M.orM $ pure <$> altList
    b <- M.orM $ pure <$> falseList
    assert' "orM alt" a
    assert' "orM false" $ not b

  test_anyPM = do
    a <- M.anyPM (const <<< pure <$> altList) unit
    b <- M.anyPM (const <<< pure <$> falseList) unit
    assert' "anyPM alt" a
    assert' "anyPM false" $ not b

  test_allPM = do
    a <- M.allPM (const <<< pure <$> trueList) unit
    b <- M.allPM (const <<< pure <$> falseList) unit
    assert' "allPM true" a
    assert' "allPM false" $ not b

  test_anyM = do
    a <- M.anyM pure altList
    b <- M.anyM pure falseList
    assert' "anyM alt" a
    assert' "anyM false" $ not b

  test_allM = do
    a <- M.allM pure trueList
    b <- M.allM pure falseList
    assert' "allM true" a
    assert' "allM false" $ not b

  issue1 =
    assert' "issue1" $ isLeft $ runExcept $ M.untilJust $ throwError unit
