module EjStand.InternalsCore(
    (|>),
    (||>),
    (==>),
    (|||),
    skipKey,
    takeFromSetBy
) where

import           Control.Applicative (liftA2)
import           Data.Set            (Set)
import qualified Data.Set            as Set

-- Function tools

(|>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(|>) f1 f2 x = f2 x <$> f1 x

(||>) :: (Functor f, Functor g) => (a -> f (g b)) -> (a -> b -> c) -> a -> f (g c)
(||>) f1 f2 x = (f2 x <$>) <$> f1 x

skipKey :: (b -> c) -> a -> b -> c
skipKey f _ = f

-- Character predicates

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) = liftA2 (||)

-- List expressions

(==>) :: Bool -> a -> [a]
(==>) False _ = []
(==>) True  x = [x]

-- Set operations

takeFromSetBy :: Ord b => (a -> b) -> b -> Set a -> Set a
takeFromSetBy f x = Set.takeWhileAntitone ((== x) . f) . Set.dropWhileAntitone ((< x) . f)
