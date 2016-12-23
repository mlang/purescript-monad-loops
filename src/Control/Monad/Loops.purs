module Control.Monad.Loops (
  whileM, whileM', whileM_, whileMR, whileMR',
  untilM, untilM', untilM_, untilMR, untilMR'
) where

import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop), tailRecM)
import Data.Monoid (class Monoid, mempty)
import Prelude ( class Applicative, class Monad, class Semigroup
               , Unit
               , append, bind, ifM, map, not, pure, unit
               , ($), (*>), (<>), (<$>), (<<<), (>>=)
               )

whileM :: forall a m. Monad m => m Boolean -> m a -> m (Array a)
whileM = whileM'

whileM' :: forall a f m. (Monad m, Applicative f, Monoid (f a))
        => m Boolean -> m a -> m (f a)
whileM' p f = ifM p
  (f >>= \ x -> whileM' p f >>= \ xs -> pure $ pure x <> xs)
  (pure mempty)

whileM_ :: forall a m. Monad m => m Boolean -> m a -> m Unit
whileM_ p f = ifM p (f *> whileM_ p f) (pure unit)

whileMR :: forall a m. MonadRec m => m Boolean -> m a -> m (Array a)
whileMR = whileMR'

whileMR' :: forall a f m. (MonadRec m, Applicative f, Monoid (f a))
         => m Boolean -> m a -> m (f a)
whileMR' p f = tailRecM go mempty where
  go xs = ifM p (f >>= pure <<< Loop <<< append xs <<< pure) (pure $ Done xs)

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

untilMR :: forall a m. MonadRec m => m a -> m Boolean -> m (Array a)
untilMR = untilMR'

untilMR' :: forall a f m. (MonadRec m, Applicative f, Semigroup (f a))
         => m a -> m Boolean -> m (f a)
untilMR' f p = f >>= tailRecM go <<< pure where
  go xs = ifM p' (f >>= pure <<< Loop <<< append xs <<< pure) (pure $ Done xs)
  p' = not <$> p
