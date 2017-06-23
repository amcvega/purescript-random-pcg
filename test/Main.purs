module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Identity
import Data.Tuple
import Data.Newtype (unwrap)

import Random.PCG

import Test.Unit
import Test.Unit.Main
import Test.Unit.Console
import Test.Unit.QuickCheck

import Test.QuickCheck (Result(..), (===), (/==))

intProperty :: Int -> Int -> Result
intProperty s1 s2 =
  let s = initialSeed (Tuple s1 s2)
      Tuple _ s' = unwrap (runIdent s)
  in s /== s'
  where
    runIdent :: Seed -> Identity (Tuple Int Seed) 
    runIdent s = runRandom randomInt s

    
numProperty :: Int -> Int -> Result
numProperty s1 s2 =
  let s = initialSeed (Tuple s1 s2)
      Tuple _ s' = unwrap (runIdent s)
  in s /== s'
  where
    runIdent :: Seed -> Identity (Tuple Number Seed) 
    runIdent s = runRandom randomNum s



main = runTest do
  test "int property" $ quickCheck intProperty
  test "num property" $ quickCheck numProperty  
  -- log "You should add some tests."
