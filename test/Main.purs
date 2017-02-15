module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Loops as M

import Data.Either (Either(Left))
import Data.Maybe (Maybe)

import Test.Assert               ( ASSERT, assert )

main :: Eff (assert :: ASSERT) Unit
main = do
  assert $ issue1 == Left ""

 where

  issue1 :: Either String (Maybe Int)
  issue1 = runExcept (M.untilJust p) where
    p = throwError ""

