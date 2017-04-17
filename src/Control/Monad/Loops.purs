module Control.Monad.Loops (
  whileM, whileM', whileM_,
  untilM, untilM', untilM_,
  iterateWhile, iterateUntil, iterateUntilM,
  whileJust, whileJust', whileJust_, untilJust,
  unfoldM, unfoldM', unfoldM_, unfoldrM, unfoldrM',
  andM, orM, anyPM, allPM, anyM, allM
) where

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(Tuple))
import Prelude ( class Applicative, class Monad
               , Unit
               , const, flip, not, pure, unit, bind
               , ($), (<$>), (<<<), (<>), (>>=)
               )

-- | Execute an action repeatedly as long as the given boolean expression
-- | returns `true`.  The condition is evaluated before the loop body.
-- | Collects the results into an `Array`.
-- | See `whileM'` for a generalized `Applicative` monoidal result.
whileM :: forall a m. Monad m => m Boolean -> m a -> m (Array a)
whileM = whileM'

-- | Execute an action repeatedly as long as the given boolean expression
-- | returns `true`.  The condition is evaluated before the loop body.
-- | Collects the results into an arbitrary `Applicative` monoidal structure.
whileM' :: forall m f a. Monad m => Applicative f => Monoid (f a)
        => m Boolean -> m a -> m (f a)
whileM' p f = loop mempty
  where
  loop acc =
    p >>= if _
      then f >>= \ x -> loop (pure x <> acc)
      else pure acc

-- | Execute an action repeatedly as long as the given boolean expression
-- | returns `true`.  The condition is evaluated before the loop body.
-- | Ignores the results of loop body execution.
whileM_ :: forall a m. Monad m => m Boolean -> m a -> m Unit
whileM_ p f = p >>= if _ then (f >>= \ _ -> whileM_ p f) else pure unit

-- | Execute an action repeatedly until the condition expression returns `true`.
-- | The condition is evaluated after the loop body.
-- | Collects results into an `Array`.
untilM :: forall a m. Monad m => m a -> m Boolean -> m (Array a)
untilM = untilM'

-- | Execute an action repeatedly until the condition expression returns `true`.
-- | The condition is evaluated after the loop body.
-- | Collects results into an arbitrary `Applicative` monoidal structure.
-- | For an `Applicative` semigroupoid result, see
-- | `Control.Monad.Rec.Loops.untilM'`.
untilM' :: forall m f a. Monad m => Applicative f => Monoid (f a)
        => m a -> m Boolean -> m (f a)
untilM' f p = loop mempty
  where
  loop acc = do
    acc' <- (_ <> acc) <<< pure <$> f
    p >>= if _ then pure acc' else loop acc'

-- | Execute an action repeatedly until the condition expression returns `true`.
-- | The condition is evaluated after the loop body.
-- | Ignores the results of loop body execution.
untilM_ :: forall m a. Monad m => m a -> m Boolean -> m Unit
untilM_ f p = f >>= \ _ -> p >>= if _ then untilM_ f p else pure unit

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- | and return that result (discarding all others).
iterateWhile :: forall a m. Monad m => (a -> Boolean) -> m a -> m a
iterateWhile p = iterateUntil $ not p

-- | Execute an action repeatedly until its result satisfies a predicate,
-- | and return that result (discarding all others).
iterateUntil :: forall a m. Monad m => (a -> Boolean) -> m a -> m a
iterateUntil p x = x >>= iterateUntilM p (const x)

-- | Yields the result of applying `f` until `p` holds.
iterateUntilM :: forall m a. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
iterateUntilM p f v = if p v then pure v else f v >>= iterateUntilM p f

-- | As long as the supplied Maybe expression returns Just, the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are collected into an array.
whileJust :: forall a b m. Monad m => m (Maybe a) -> (a -> m b) -> m (Array b)
whileJust = whileJust'

-- | As long as the supplied `Maybe` expression returns `Just`, the loop
-- | body will be called and passed the value contained in the `Just`.
-- | Results are collected into an arbitrary `Applicative` monoidal structure.
whileJust' :: forall m a f b. Monad m => Applicative f => Monoid (f b)
           => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust' p f =
  p >>= maybe (pure mempty)
              (\ v -> f v >>= \ x -> whileJust' p f >>= pure <<< (pure x <> _))

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are discarded.
whileJust_ :: forall a b m. Monad m => m (Maybe a) -> (a -> m b) -> m Unit
whileJust_ p f = p >>= maybe (pure unit) (\ v -> f v >>= \ _ -> whileJust_ p f)

-- | Run the supplied Maybe computation repeatedly until it returns a
-- | value.  Returns that value.
untilJust :: forall a m. Monad m => m (Maybe a) -> m a
untilJust m = m >>= \ x -> maybe (untilJust m) pure x

-- | The supplied Maybe expression will be repeatedly called until it
-- | returns `Nothing`.  All values returned are collected into an `Array`.
unfoldM :: forall a m. Monad m => m (Maybe a) -> m (Array a)
unfoldM = unfoldM'

-- | The supplied `Maybe` expression will be repeatedly called until it
-- | returns `Nothing`.
-- | All `Just` values are collected into an `Applicative` monoidal structure.
unfoldM' :: forall m f a. Monad m => Applicative f => Monoid (f a)
         => m (Maybe a) -> m (f a)
unfoldM' = flip whileJust' pure

-- | The supplied Maybe expression will be repeatedly called until it
-- | returns Nothing.  All values returned are discarded.
unfoldM_ :: forall a m. Monad m => m (Maybe a) -> m Unit
unfoldM_ = flip whileJust_ pure

unfoldrM :: forall a b m. Monad m
         => (a -> m (Maybe (Tuple b a))) -> a -> m (Array b)
unfoldrM = unfoldrM'

-- | See 'Data.List.unfoldr'.  This is a monad-friendly version of that, with a
-- | twist.  Rather than returning a list, it returns any MonadPlus type of your
-- | choice.
unfoldrM' :: forall a b f m. Monad m => Applicative f => Monoid (f b)
          => (a -> m (Maybe (Tuple b a))) -> a -> m (f b)
unfoldrM' f = go where
  go z = f z >>= maybe (pure mempty)
                       (\ (Tuple x z') -> go z' >>= pure <<< (pure x <> _))

-- | short-circuit 'and' for monadic boolean values.
andM :: forall m. (Monad m) => List (m Boolean) -> m Boolean
andM Nil    = pure true
andM (p:ps) = p >>= if _ then andM ps else pure false

-- | short-circuit 'or' for values of type Monad m => m Bool
orM :: forall m. Monad m => List (m Boolean) -> m Boolean
orM Nil    = pure false
orM (p:ps) = p >>= if _ then pure true else orM ps

-- | short-circuit 'any' with a list of \"monadic predicates\".  Tests the
-- | value presented against each predicate in turn until one passes, then
-- | returns True without any further processing.  If none passes, returns False.
anyPM :: forall m a. Monad m => List (a -> m Boolean) -> (a -> m Boolean)
anyPM Nil    _ = pure false
anyPM (p:ps) x = p x >>= if _ then pure true else anyPM ps x

-- | short-circuit 'all' with a list of \"monadic predicates\".  Tests the value
-- | presented against each predicate in turn until one fails, then returns False.
-- | if none fail, returns True.
allPM :: forall m a. Monad m => List (a -> m Boolean) -> (a -> m Boolean)
allPM Nil    _ = pure true
allPM (p:ps) x = p x >>= if _ then allPM ps x else pure false

-- | short-circuit 'any' with a \"monadic predicate\".
anyM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m Boolean
anyM _ Nil    = pure false
anyM p (x:xs) = p x >>= if _ then pure true else anyM p xs

-- | short-circuit 'all' with a \"monadic predicate\".
allM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m Boolean
allM _ Nil    = pure true
allM p (x:xs) = p x >>= if _ then allM p xs else pure false
