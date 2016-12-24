module Control.Monad.Rec.Loops (
  whileM, whileM', whileM_,
  untilM, untilM', untilM_,
  iterateWhile, iterateUntil, iterateUntilM,
  whileJust, whileJust', whileJust_, untilJust
) where

import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop), tailRecM)
import Data.Functor (voidLeft)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Prelude ( class Applicative, class Semigroup
               , Unit
               , append, const, flip, ifM, not, pure, unit
               , ($), ($>), (*>), (<$>), (<*>), (<<<), (>>=)
               )

whileM :: forall a m. MonadRec m => m Boolean -> m a -> m (Array a)
whileM = whileM'

whileM' :: forall a f m. (MonadRec m, Applicative f, Monoid (f a))
         => m Boolean -> m a -> m (f a)
whileM' p f = tailRecM (ifM p <$> collect f <*> done) mempty

whileM_ :: forall a m. MonadRec m => m Boolean -> m a -> m Unit
whileM_ p f = tailRecM (ifM p <$> voidLeft f <<< Loop <*> done) unit

untilM :: forall a m. MonadRec m => m a -> m Boolean -> m (Array a)
untilM = untilM'

untilM' :: forall a f m. (MonadRec m, Applicative f, Semigroup (f a))
         => m a -> m Boolean -> m (f a)
untilM' f p = f >>= tailRecM (ifM p <$> done <*> collect f) <<< pure

untilM_ :: forall a m. MonadRec m => m a -> m Boolean -> m Unit
untilM_ f p = f *> tailRecM (ifM p <$> done <*> voidLeft f <<< Loop) unit

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- | and return that result (discarding all others).
iterateWhile :: forall a m. MonadRec m => (a -> Boolean) -> m a -> m a
iterateWhile p = iterateUntil $ not <<< p

-- | Yields the result of applying f until p holds.
iterateUntilM :: forall a m. MonadRec m
              => (a -> Boolean) -> (a -> m a) -> a -> m a
iterateUntilM p f = tailRecM (\ v -> if p v then pure $ Done v else Loop <$> f v)

-- | Execute an action repeatedly until its result satisfies a predicate,
-- | and return that result (discarding all others).
iterateUntil :: forall a m. MonadRec m => (a -> Boolean) -> m a -> m a
iterateUntil p x = x >>= iterateUntilM p (const x)

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are collected into an array.
whileJust :: forall a b m. MonadRec m => m (Maybe a) -> (a -> m b) -> m (Array b)
whileJust = whileJust'

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are collected into an arbitrary MonadPlus container.
whileJust' :: forall a b f m. (MonadRec m, Applicative f, Monoid (f b))
           => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust' p f =
  tailRecM (\ xs -> p >>= maybe (done xs) (flip collect xs <<< f)) mempty

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are discarded.
whileJust_ :: forall a b m. MonadRec m => m (Maybe a) -> (a -> m b) -> m Unit
whileJust_ p f =
  tailRecM (const $ p >>= maybe (done unit) (\ v -> f v $> Loop unit)) unit

-- | Run the supplied "Maybe" computation repeatedly until it returns a
-- | value.  Returns that value.
untilJust :: forall a m. MonadRec m => m (Maybe a) -> m a
untilJust m =
  tailRecM (const $ m >>= maybe (pure $ Loop unit) (pure <<< Done)) unit

-------------------------------------------------------------------------------

collect :: forall a b f m. (MonadRec m, Applicative f, Semigroup (f a))
        => m a -> f a -> m (Step (f a) b)
collect f xs = f >>= pure <<< Loop <<< append xs <<< pure

done :: forall a b m. MonadRec m => b -> m (Step a b)
done = pure <<< Done
