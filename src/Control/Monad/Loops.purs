module Control.Monad.Loops (
  whileM, whileM', whileM_,
  untilM, untilM', untilM_,
  iterateWhile, iterateUntil, iterateUntilM
) where

import Data.Monoid (class Monoid, mempty)
import Prelude ( class Applicative, class Monad
               , Unit
               , append, const, ifM, not, pure, unit
               , (*>), (<$>), (<<<), (>>=)
               )

whileM :: forall a m. Monad m => m Boolean -> m a -> m (Array a)
whileM = whileM'

whileM' :: forall a f m. (Monad m, Applicative f, Monoid (f a))
        => m Boolean -> m a -> m (f a)
whileM' p f = ifM p
  (f >>= \ x -> whileM' p f >>= pure <<< append (pure x))
  (pure mempty)

whileM_ :: forall a m. Monad m => m Boolean -> m a -> m Unit
whileM_ p f = ifM p (f *> whileM_ p f) (pure unit)

untilM :: forall a m. Monad m => m a -> m Boolean -> m (Array a)
untilM = untilM'

untilM' :: forall a f m. (Monad m, Applicative f, Monoid (f a))
        => m a -> m Boolean -> m (f a)
untilM' f p = f >>= \ x -> whileM' (not <$> p) f >>= pure <<< append (pure x)

untilM_ :: forall a m. Monad m => m a -> m Boolean -> m Unit
untilM_ f p = f *> whileM_ p f

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- and return that result (discarding all others).
iterateWhile :: forall a m. Monad m => (a -> Boolean) -> m a -> m a
iterateWhile p = iterateUntil (not <<< p)

-- | Yields the result of applying f until p holds.
iterateUntilM :: forall a m. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
iterateUntilM p f v = if p v then pure v else f v >>= iterateUntilM p f

-- | Execute an action repeatedly until its result satisfies a predicate,
-- and return that result (discarding all others).
iterateUntil :: forall a m. Monad m => (a -> Boolean) -> m a -> m a
iterateUntil p x = x >>= iterateUntilM p (const x)
