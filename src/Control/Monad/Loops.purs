module Control.Monad.Loops (
  whileM, whileM', whileM_,
  untilM, untilM', untilM_
) where

import Data.Monoid (class Monoid, mempty)
import Prelude (class Applicative, class Monad, Unit, bind, ifM, map, not, pure, unit, ($), (*>), (<>), (>>=))

whileM :: forall a m. Monad m => m Boolean -> m a -> m (Array a)
whileM = whileM'

whileM' :: forall a f m. (Monad m, Applicative f, Monoid (f a))
        => m Boolean -> m a -> m (f a)
whileM' p f = ifM p
  (f >>= \ x -> whileM' p f >>= \ xs -> pure $ pure x <> xs)
  (pure mempty)

whileM_ :: forall a m. Monad m => m Boolean -> m a -> m Unit
whileM_ p f = ifM p (f *> whileM_ p f) (pure unit)

untilM :: forall a m. Monad m => m a -> m Boolean -> m (Array a)
untilM = untilM'

untilM' :: forall a f m. (Monad m, Applicative f, Monoid (f a))
        => m a -> m Boolean -> m (f a)
untilM' f p = do
  x <- f
  xs <- whileM' (map not p) f
  pure $ pure x <> xs

untilM_ :: forall a m. Monad m => m a -> m Boolean -> m Unit
untilM_ f p = f *> whileM_ p f
