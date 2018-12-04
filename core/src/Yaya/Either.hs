-- | Operations on partial-function-like structures.
module Yaya.Either where

import Control.Monad
import Prelude hiding (until)

import Yaya
import Yaya.Control

now :: Embeddable t (Either a) => a -> t
now = embed . Left

never :: Coalgebra (Either b) a
never = Right

-- | Returns the last 'Just' result.
while :: (a -> Maybe a) -> Coalgebra (Either a) a
while f a = maybe (Left a) Right $ f a

-- | Runs 'n' steps or until there’s a result, whichever is less.
runFor :: Projectable t (Either a) => Algebra Maybe (Coalgebra (Either a) t)
runFor Nothing  = project
runFor (Just ψ) = project <=< ψ

-- threshold = cata runFor 10
-- tolerance = cata runFor 5

-- | So, the idea here is that you can log some value warning that you’ve
--   crossed some threshold, and then actually fail if you cross a higher one.
-- either (\out -> println "success " ++ out)
--        (const $ println "too hot")
--   . (println "getting hot" *> tolerance) <=< threshold

-- | Runs until the predicate is true.
until :: (a -> a) -> (a -> Bool) -> Coalgebra (Either a) a
until f p a = if p a then Left a else Right $ f a

-- | Runs until the result matches the input.
fixed :: Eq a => (a -> a) -> Coalgebra (Either a) a
fixed f = f `until` ((==) <*> f)

-- | Adds an index to a partial function, to count how many recursive calls were
--   needed to reach a result.
-- countSteps
--   :: (Embeddable n Maybe, MonadWriter n m)
--   => Coalgebra (Either a) a
--   -> CoalgebraM m (Either a) a
-- countSteps ψ = censor succN . pure . ψ

-- | This will collapse all the intermediate steps to get to the value that must
--   exist at the end.
runToEnd :: Recursive t (Either a) => t -> a
runToEnd = cata fromEither
  where
    fromEither (Left a) = a
    fromEither (Right a) = a

-- | Converts exceptional divergence to non-termination.
fromMaybe :: (Embeddable t (Either a), Corecursive t (Either a)) => Maybe a -> t
fromMaybe = maybe (ana never ()) now
