module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Loops as M

import Data.Either (isLeft)

import Test.Assert               ( ASSERT, assert )

main :: Eff (assert :: ASSERT) Unit
main = do
  issue1

 where

  issue1 = assert $ isLeft $ runExcept $ M.untilJust $ throwError unit
