module Control.Monad.Loops (
  whileM, whileM', whileM_, whileMR, whileMR', whileMR_,
  untilM, untilM', untilM_, untilMR, untilMR', untilMR_
) where

import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop), tailRecM)
import Data.Monoid (class Monoid, mempty)
import Prelude ( class Applicative, class Monad, class Semigroup
               , Unit
               , append, bind, const, flip, ifM, not, pure, unit
               , ($), ($>), (*>), (<$>), (<<<), (>>=)
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

whileMR :: forall a m. MonadRec m => m Boolean -> m a -> m (Array a)
whileMR = whileMR'

whileMR' :: forall a f m. (MonadRec m, Applicative f, Monoid (f a))
         => m Boolean -> m a -> m (f a)
whileMR' p f = tailRecM (\ xs -> ifM p (collect xs f) (done xs)) mempty

whileMR_ :: forall a m. MonadRec m => m Boolean -> m a -> m Unit
whileMR_ p f = tailRecM (const $ ifM p (f $> Loop unit) (done unit)) unit

untilM :: forall a m. Monad m => m a -> m Boolean -> m (Array a)
untilM = untilM'

untilM' :: forall a f m. (Monad m, Applicative f, Monoid (f a))
        => m a -> m Boolean -> m (f a)
untilM' f p = f >>= \ x -> whileM' (not <$> p) f >>= pure <<< append (pure x)

untilM_ :: forall a m. Monad m => m a -> m Boolean -> m Unit
untilM_ f p = f *> whileM_ p f

untilMR :: forall a m. MonadRec m => m a -> m Boolean -> m (Array a)
untilMR = untilMR'

untilMR' :: forall a f m. (MonadRec m, Applicative f, Semigroup (f a))
         => m a -> m Boolean -> m (f a)
untilMR' f p = f >>= tailRecM (\ xs -> ifM p (done xs) (collect xs f)) <<< pure

untilMR_ :: forall a m. MonadRec m => m a -> m Boolean -> m Unit
untilMR_ f p = f *> tailRecM (const $ ifM p (done unit) (f $> Loop unit)) unit

-------------------------------------------------------------------------------

collect :: forall a b f m. (MonadRec m, Applicative f, Semigroup (f a))
        => f a -> m a -> m (Step (f a) b)
collect xs = flip bind $ pure <<< Loop <<< append xs <<< pure

done :: forall a b m. MonadRec m => b -> m (Step a b)
done = pure <<< Done
