module Control.Monad.Rec.Loops (
  whileM, whileM', whileM_,
  untilM, untilM', untilM_,
  iterateWhile, iterateUntil, iterateUntilM,
  whileJust, whileJust', whileJust_, untilJust,
  unfoldM, unfoldM', unfoldM_, unfoldrM, unfoldrM',
  andM, orM, anyPM, allPM, anyM, allM
) where

import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(Done, Loop), tailRecM, tailRecM2)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(Tuple))
import Prelude ( class Apply, class Applicative, class Bind, class Semigroup
               , Unit
               , bind, const, flip, ifM, not, pure, unit
               , ($), ($>), (*>), (<$>), (<<<), (<>), (>>=)
               )

-- | Execute an action repeatedly as long as the given boolean expression
-- | returns `true`.  The condition is evaluated before the loop body.
-- | Collects the results into an `Array`.
-- | See `whileM'` for a generalized `Applicative` monoidal result.
whileM :: forall a m. MonadRec m => m Boolean -> m a -> m (Array a)
whileM = whileM'

-- | Execute an action repeatedly as long as the given boolean expression
-- | returns `true`.  The condition is evaluated before the loop body.
-- | Collects the results into an arbitrary `Applicative` monoidal structure.
whileM' :: forall a f m. MonadRec m => Applicative f => Monoid (f a)
        => m Boolean -> m a -> m (f a)
whileM' p f = tailRecM (liftIfM p (_ <+> f) done) mempty

-- | Execute an action repeatedly as long as the given boolean expression
-- | returns `true`.  The condition is evaluated before the loop body.
-- | Ignores the results of loop body execution.
whileM_ :: forall a m. MonadRec m => m Boolean -> m a -> m Unit
whileM_ p f = tailRecM (liftIfM p ((f $> _) <<< Loop) done) unit

-- | Execute an action repeatedly until the condition expression returns `true`.
-- | The condition is evaluated after the loop body.
-- | Collects results into an `Array`.
untilM :: forall a m. MonadRec m => m a -> m Boolean -> m (Array a)
untilM = untilM'

-- | Execute an action repeatedly until the condition expression returns `true`.
-- | The condition is evaluated after the loop body.
-- | Collects results into an arbitrary `Applicative` semigroupoid structure.
untilM' :: forall a f m. MonadRec m => Applicative f => Semigroup (f a)
         => m a -> m Boolean -> m (f a)
untilM' f p = f >>= tailRecM (liftIfM p done (_ <+> f)) <<< pure

-- | Execute an action repeatedly until the condition expression returns `true`.
-- | The condition is evaluated after the loop body.
-- | Ignores the results of loop body execution.
untilM_ :: forall a m. MonadRec m => m a -> m Boolean -> m Unit
untilM_ f p = f *> tailRecM (liftIfM p done $ (f $> _) <<< Loop) unit

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- | and return that result (discarding all others).
iterateWhile :: forall a m. MonadRec m => (a -> Boolean) -> m a -> m a
iterateWhile p = iterateUntil $ not p

-- | Execute an action repeatedly until its result satisfies a predicate,
-- | and return that result (discarding all others).
iterateUntil :: forall a m. MonadRec m => (a -> Boolean) -> m a -> m a
iterateUntil p x = x >>= iterateUntilM p (const x)

-- | Yields the result of applying f until p holds.
iterateUntilM :: forall a m. MonadRec m
              => (a -> Boolean) -> (a -> m a) -> a -> m a
iterateUntilM p f = tailRecM \ v -> if p v then done v else Loop <$> f v

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are collected into an array.
whileJust :: forall a b m. MonadRec m => m (Maybe a) -> (a -> m b) -> m (Array b)
whileJust = whileJust'

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are collected into an arbitrary MonadPlus container.
whileJust' :: forall a b f m. MonadRec m => Applicative f => Monoid (f b)
           => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust' p f = tailRecM go mempty where
  go xs = p >>= maybe (done xs) ((xs <+> _) <<< f)

-- | As long as the supplied "Maybe" expression returns "Just _", the loop
-- | body will be called and passed the value contained in the 'Just'.  Results
-- | are discarded.
whileJust_ :: forall a b m. MonadRec m => m (Maybe a) -> (a -> m b) -> m Unit
whileJust_ p f =
  tailRecM (const $ p >>= maybe (done unit) (\ v -> f v $> Loop unit)) unit

-- | Run the supplied "Maybe" computation repeatedly until it returns a
-- | value.  Returns that value.
untilJust :: forall a m. MonadRec m => m (Maybe a) -> m a
untilJust m = tailRecM (const $ m >>= maybe (loop unit) done) unit

-- | The supplied Maybe expression will be repeatedly called until it
-- | returns Nothing.  All values returned are collected into an array.
unfoldM :: forall a m. MonadRec m => m (Maybe a) -> m (Array a)
unfoldM = unfoldM'

-- | The supplied Maybe expression will be repeatedly called until it
-- | returns Nothing.  All values returned are collected into an Applicative Monoid.
unfoldM' :: forall a f m. MonadRec m => Applicative f => Monoid (f a)
         => m (Maybe a) -> m (f a)
unfoldM' = flip whileJust' pure

-- | The supplied Maybe expression will be repeatedly called until it
-- | returns Nothing.  All values returned are discarded.
unfoldM_ :: forall a m. MonadRec m => m (Maybe a) -> m Unit
unfoldM_ = flip whileJust_ pure

unfoldrM :: forall a b m. MonadRec m
         => (a -> m (Maybe (Tuple b a))) -> a -> m (Array b)
unfoldrM = unfoldrM'

-- | See 'Data.List.unfoldr'.  This is a monad-friendly version of that, with a
-- | twist.  Rather than returning a list, it returns any MonadPlus type of your
-- | choice.
unfoldrM' :: forall a b f m. MonadRec m => Applicative f => Monoid (f b)
          => (a -> m (Maybe (Tuple b a))) -> a -> m (f b)
unfoldrM' f = tailRecM2 go mempty where
  go xs z = bind (f z) $ maybe (done xs) $
            \ (Tuple x z') -> loop { a: xs <> pure x, b: z' }

-- | short-circuit 'and' for monadic boolean values.
andM :: forall m. (MonadRec m) => List (m Boolean) -> m Boolean
andM = tailRecM go where
  go Nil    = done true
  go (p:ps) = ifM p (loop ps) (done false)

-- | short-circuit 'or' for values of type Monad m => m Bool
orM :: forall m. MonadRec m => List (m Boolean) -> m Boolean
orM = tailRecM go where
  go Nil    = done false
  go (p:ps) = ifM p (done true) (loop ps)

-- | short-circuit 'any' with a list of \"monadic predicates\".  Tests the
-- | value presented against each predicate in turn until one passes, then
-- | returns True without any further processing.  If none passes, returns False.
anyPM :: forall m a. MonadRec m => List (a -> m Boolean) -> (a -> m Boolean)
anyPM mps x = tailRecM go mps where
  go Nil    = done false
  go (p:ps) = ifM (p x) (done true) (loop ps)

-- | short-circuit 'all' with a list of \"monadic predicates\".  Tests the value
-- | presented against each predicate in turn until one fails, then returns False.
-- | if none fail, returns True.
allPM :: forall a m. MonadRec m => List (a -> m Boolean) -> (a -> m Boolean)
allPM mps x = tailRecM go mps where
  go Nil    = done true
  go (p:ps) = ifM (p x) (loop ps) (done false)

-- | short-circuit 'any' with a \"monadic predicate\".
anyM :: forall a m. MonadRec m => (a -> m Boolean) -> List a -> m Boolean
anyM p = tailRecM go where
  go Nil    = done false
  go (x:xs) = ifM (p x) (done true) (loop xs)

-- | short-circuit 'all' with a \"monadic predicate\".
allM :: forall m a. MonadRec m => (a -> m Boolean) -> List a -> m Boolean
allM p = tailRecM go where
  go Nil    = done true
  go (x:xs) = ifM (p x) (loop xs) (done false)

-------------------------------------------------------------------------------

liftIfM :: forall m f a. Apply f => Bind m
        => m Boolean -> f (m a) -> f (m a) -> f (m a)
liftIfM = lift2 <<< ifM

-- | Append the result of running a monadic computation to an applicative semigroup
-- | and return it as a loop continuation for `tailRecM`.
appendM :: forall m f a b. MonadRec m => Applicative f => Semigroup (f a)
        => f a -> m a -> m (Step (f a) b)
appendM xs f = f >>= loop <<< (xs <> _) <<< pure

infixr 5 appendM as <+>

loop :: forall m a b. MonadRec m => a -> m (Step a b)
loop = pure <<< Loop

done :: forall m a b. MonadRec m => b -> m (Step a b)
done = pure <<< Done
