module Random.PCG (GeneratorT(..), Seed(..), initialSeed, runRandom, randomInt, array, list, randomNum) where

import Prelude

import Control.Monad.State.Trans (class MonadTrans, StateT, get, lift, put, runStateT)

import Data.List (List, foldM, fromFoldable, range, (:))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Identity (Identity)



data Seed = Seed Int Int

instance eqSeed :: Eq Seed where
  eq (Seed x1 x2) (Seed y1 y2)  = [x1,x2] == [y1,y2]

-- type Int64 = {msb :: Int, lsb :: Int}

newtype GeneratorT m a = GeneratorT (StateT Seed m a)

type Generator a = GeneratorT Identity a

instance monadTransGeneratorT :: MonadTrans GeneratorT where
  lift m = GeneratorT $ lift m
  
instance functorGeneratorT :: Functor m => Functor (GeneratorT m) where
  map fn (GeneratorT s) = GeneratorT $ map fn s

instance applyGeneratorT :: Monad m => Apply (GeneratorT m) where
  apply (GeneratorT f) (GeneratorT v) = GeneratorT $ f <*> v

instance applicativeGeneratorT :: Monad m => Applicative (GeneratorT m) where
  pure = GeneratorT <<< pure 

  
instance bindGeneratorT :: Monad m => Bind (GeneratorT m) where
  bind (GeneratorT s) f =
    GeneratorT $ s >>= \x -> case f x of GeneratorT new -> new

instance monadGeneratorT :: Monad m => Monad (GeneratorT m)                                         

instance showSeed :: Show Seed where
  show (Seed x y) = "Seed " <> show x <> " - " <> show y

type State = {hi :: Int, lo :: Int}

type Answer a = { answer :: a, state :: State }


initialSeed :: Tuple Int Int -> Seed
initialSeed (Tuple x y) = Seed x y


foreign import randomIntImpl :: State -> Answer Int

foreign import randomNumberImpl :: State -> Answer Number


-- |Run a generator with a given seed. Returns a Tuple of result and seed.
runRandom :: ∀ m a. GeneratorT m a -> Seed -> m (Tuple a Seed)
runRandom (GeneratorT state) = runStateT state


-- | Generate random integer
randomInt :: ∀ m. Monad m => GeneratorT m Int
randomInt = GeneratorT do
   Seed x y <- get
   let ans = randomIntImpl {hi: x, lo: y} 
   put $ Seed ans.state.hi ans.state.lo
   pure ans.answer

-- | Generate a random number
randomNum :: ∀ m. Monad m => GeneratorT m Number
randomNum = GeneratorT do
  Seed x y <- get
  let ans = randomNumberImpl {hi: x, lo: y}
  put $ Seed ans.state.hi ans.state.lo
  pure ans.answer


-- | Generate an Array of random elements
array :: ∀ m a. Monad m
         => Int
         -> GeneratorT m a
         -> GeneratorT m (Array a)
array n fn = Array.foldM go [] (Array.range 1 n)
  where
    go xs _ = do
      res <- fn
      pure (Array.cons res xs)


-- | Generate a list of random elements
list :: ∀ m a. Monad m
        => Int
        -> GeneratorT m a
        -> GeneratorT m (List a)
list n fn = foldM go (fromFoldable []) (range 1 n)
  where
    go xs _ = do
      res <- fn
      pure (res:xs)
