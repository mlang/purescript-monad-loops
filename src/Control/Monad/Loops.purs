module Control.Monad.Loops (
  whileM, whileM', whileM_,
  untilM, untilM', untilM_,
  iterateWhile, iterateUntil, iterateUntilM,
  whileJust, whileJust', whileJust_, untilJust
) where

import Data.Maybe (Maybe, maybe)
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

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are collected into an array.
whileJust :: forall a b m. Monad m => m (Maybe a) -> (a -> m b) -> m (Array b)
whileJust = whileJust'

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are collected into an arbitrary MonadPlus container.
whileJust' :: forall a b f m. (Monad m, Applicative f, Monoid (f b))
           => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust' p f =
  p >>= maybe (pure mempty)
              (\ v -> f v >>= \ x -> whileJust' p f >>= pure <<< append (pure x))

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- body will be called and passed the value contained in the 'Just'.  Results
-- are discarded.
whileJust_ :: forall a b m. Monad m => m (Maybe a) -> (a -> m b) -> m Unit
whileJust_ p f = p >>= maybe (pure unit) (\ v -> f v *> whileJust_ p f)

-- | Run the supplied "Maybe" computation repeatedly until it returns a
-- value.  Returns that value.
untilJust :: forall a m. Monad m => m (Maybe a) -> m a
untilJust m = m >>= maybe (untilJust m) pure
