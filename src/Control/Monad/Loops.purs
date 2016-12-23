module Control.Monad.Loops (whileM, whileM_, untilM, untilM_) where

import Prelude

whileM :: forall a m. Monad m => m Boolean -> m a -> m (Array a)
whileM p f = ifM p
  (f >>= \ x -> whileM p f >>= \ xs -> pure $ [x] <> xs)
  (pure [])

whileM_ :: forall a m. Monad m => m Boolean -> m a -> m Unit
whileM_ p f = ifM p (f *> whileM_ p f) (pure unit)

untilM :: forall a m. Monad m => m a -> m Boolean -> m (Array a)
untilM f p = do
  x <- f
  xs <- whileM (map not p) f
  pure $ [x] <> xs

untilM_ :: forall a m. Monad m => m a -> m Boolean -> m Unit
untilM_ f p = f *> whileM_ p f
