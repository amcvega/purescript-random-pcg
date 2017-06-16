module Random.PCG where

import Unsafe.Coerce (unsafeCoerce)
import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.State.Trans

import Data.List
import Data.Tuple
import Data.Array as Array
import Data.Identity



data Seed = Seed Int Int

type Int64 = {msb :: Int, lsb :: Int}

-- newtype Generator a = Generator {seed :: Seed}
newtype GeneratorT m a = GeneratorT (StateT Seed m a)

type Generator a = GeneratorT Identity a

-- instance bindGenerator :: Bind Generator where
--   bind (GeneratorT s) fn = 
instance monadTransGeneratorT :: MonadTrans GeneratorT where
  lift m = GeneratorT $ lift m
  
instance functorGeneratorT :: Functor m => Functor (GeneratorT m) where
  map fn (GeneratorT s) = GeneratorT $ map fn s

instance applyGeneratorT :: Monad m => Apply (GeneratorT m) where
  apply (GeneratorT f) (GeneratorT v) = GeneratorT $ f <*> v

instance applicativeGeneratorT :: Monad m => Applicative (GeneratorT m) where
  pure x = GeneratorT $ pure x

  
instance bindGeneratorT :: Monad m => Bind (GeneratorT m) where
  bind (GeneratorT s) f =
    GeneratorT $ s >>= \x -> case f x of GeneratorT new -> new

instance monadGeneratorT :: Monad m => Monad (GeneratorT m)                                         

foreign import data CRYPTO :: Effect

foreign import rand64Impl :: ∀ e. Eff (crypto :: CRYPTO | e) Int64

randomSeed :: ∀ e. Eff (crypto :: CRYPTO|e) Seed
randomSeed = do
  i <- rand64Impl
  pure $ Seed i.msb i.lsb

instance showSeed :: Show Seed where
  show (Seed x y) = "Seed " <> show x <> " - " <> show y

type State = {hi :: Int, lo :: Int}

type Answer a = { answer :: a, state :: State }

initialSeed :: Tuple Int Int -> Seed
initialSeed (Tuple x y) = Seed x y


foreign import randomIntImpl :: Array Int -> Answer Int

randomInt :: Seed -> Tuple Int Seed
randomInt (Seed x y) =
  let ans = randomIntImpl ( Array.fromFoldable [x,y])
  in Tuple (ans.answer) (Seed ans.state.hi ans.state.lo)


runRandom :: ∀ m a. GeneratorT m a -> Seed -> m (Tuple a Seed)
runRandom (GeneratorT state) = runStateT state

randInt :: ∀ m. Monad m => GeneratorT m Int
randInt = GeneratorT do
   seed <- get
   let Tuple ans seed' = randomInt seed
   put seed'
   pure ans


array :: ∀ m a. Monad m
         => Int
         -> GeneratorT m a
         -> GeneratorT m (Array a)
array n fn = Array.foldM go [] (Array.range 1 4)
  where
    go xs _ = do
      res <- fn
      pure (Array.cons res xs)


list :: ∀ m a. Monad m
        => Int
        -> GeneratorT m a
        -> GeneratorT m (List a)
list n fn = foldM go (fromFoldable []) (range 1 4)
  where
    go xs _ = do
      res <- fn
      pure (res:xs)
