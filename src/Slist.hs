{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Slist
       ( -- * Types
         Size
       , Slist
         -- ** Smart constructors
       , slist
       , infiniteSlist
       , one
         -- * Basic functions
       , size
       , isNull
       , head
       , safeHead
       , last
       , safeLast
       , init
       , tail
       , uncons

       , map
       ) where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Prelude hiding (head, init, last, map, tail)

import qualified Data.Foldable as F (Foldable (..))
import qualified GHC.Exts as L (IsList (..))
import qualified Prelude as P


data Size
    = Size !Int
    | Infinity
    deriving (Show, Read, Eq, Ord)

instance Num Size where
    (+) :: Size -> Size -> Size
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    (Size x) + (Size y) = Size (x + y)
    {-# INLINE (+) #-}

    (-) :: Size -> Size -> Size
    Infinity - _ = Infinity
    _ - Infinity = Infinity
    (Size x) - (Size y) = Size (x - y)
    {-# INLINE (-) #-}

    (*) :: Size -> Size -> Size
    Infinity * _ = Infinity
    _ * Infinity = Infinity
    (Size x) * (Size y) = Size (x * y)
    {-# INLINE (*) #-}

    abs :: Size -> Size
    abs Infinity = Infinity
    abs (Size x) = Size $ abs x
    {-# INLINE abs #-}

    signum :: Size -> Size
    signum Infinity = Infinity
    signum (Size x) = Size (signum x)
    {-# INLINE signum #-}

    fromInteger :: Integer -> Size
    fromInteger = Size . fromInteger
    {-# INLINE fromInteger #-}

data Slist a = Slist
    { sList :: [a]
    , sSize :: !Size
    } deriving (Show, Read)

instance (Eq a) => Eq (Slist a) where
    (Slist l1 s1) == (Slist l2 s2) = s1 == s2 && l1 == l2
    {-# INLINE (==) #-}

instance (Ord a) => Ord (Slist a) where
    compare (Slist l1 _) (Slist l2 _) = compare l1 l2
    {-# INLINE compare #-}

instance Semigroup (Slist a) where
    (<>) :: Slist a -> Slist a -> Slist a
    (Slist l1 s1) <> (Slist l2 s2) = Slist (l1 <> l2) (s1 + s2)
    {-# INLINE (<>) #-}

instance Monoid (Slist a) where
    mempty :: Slist a
    mempty = Slist [] 0
    {-# INLINE mempty #-}

    mappend :: Slist a -> Slist a -> Slist a
    mappend = (<>)
    {-# INLINE mappend #-}

    mconcat :: [Slist a] -> Slist a
    mconcat ls = let (l, s) = foldr f ([], 0) ls in Slist l s
      where
        -- foldr :: (a -> ([a], Size) -> ([a], Size)) -> ([a], Size) -> [Slist a] -> ([a], Size)
        f :: Slist a -> ([a], Size) -> ([a], Size)
        f (Slist l s) (xL, !xS) = (xL ++ l, s + xS)
    {-# INLINE mconcat #-}

instance Functor Slist where
    fmap :: (a -> b) -> Slist a -> Slist b
    fmap = map
    {-# INLINE fmap #-}

instance Applicative Slist where
    pure :: a -> Slist a
    pure = one
    {-# INLINE pure #-}

    (<*>) :: Slist (a -> b) -> Slist a -> Slist b
    fsl <*> sl = Slist
        { sList = sList fsl <*> sList sl
        , sSize = sSize fsl  *  sSize sl
        }
    {-# INLINE (<*>) #-}

    liftA2 :: (a -> b -> c) -> Slist a -> Slist b -> Slist c
    liftA2 f sla slb = Slist
        { sList = liftA2 f (sList sla) (sList slb)
        , sSize = sSize sla * sSize slb
        }
    {-# INLINE liftA2 #-}

instance Alternative Slist where
    empty :: Slist a
    empty = mempty
    {-# INLINE empty #-}

    (<|>) :: Slist a -> Slist a -> Slist a
    (<|>) = (<>)
    {-# INLINE (<|>) #-}

instance Monad Slist where
    return :: a -> Slist a
    return = pure
    {-# INLINE return #-}

    (>>=) :: Slist a -> (a -> Slist b) -> Slist b
    sl >>= f = mconcat $ P.map f $ sList sl
    {-# INLINE (>>=) #-}

instance Foldable Slist where
    foldMap :: (Monoid m) => (a -> m) -> Slist a -> m
    foldMap f = foldMap f . sList
    {-# INLINE foldMap #-}

    foldr :: (a -> b -> b) -> b -> Slist a -> b
    foldr f b = foldr f b . sList
    {-# INLINE foldr #-}

    elem :: (Eq a) => a -> Slist a -> Bool
    elem a = elem a . sList
    {-# INLINE elem #-}

    maximum :: (Ord a) => Slist a -> a
    maximum = maximum . sList
    {-# INLINE maximum #-}

    minimum :: (Ord a) => Slist a -> a
    minimum = minimum . sList
    {-# INLINE minimum #-}

    sum :: (Num a) => Slist a -> a
    sum = sum . sList
    {-# INLINE sum #-}

    product :: (Num a) => Slist a -> a
    product = product . sList
    {-# INLINE product #-}

    null :: Slist a -> Bool
    null = isNull
    {-# INLINE null #-}

    length :: Slist a -> Int
    length = size
    {-# INLINE length #-}

    toList :: Slist a -> [a]
    toList = sList
    {-# INLINE toList #-}

instance Traversable Slist where
    traverse :: (Applicative f) => (a -> f b) -> Slist a -> f (Slist b)
    traverse f (Slist l s) = (\x -> Slist x s) <$> traverse f l
    {-# INLINE traverse #-}

instance L.IsList (Slist a) where
    type (Item (Slist a)) = a
    fromList :: [a] -> Slist a
    fromList = slist
    {-# INLINE fromList #-}

    toList :: Slist a -> [a]
    toList = sList
    {-# INLINE toList #-}

slist :: [a] -> Slist a
slist l = Slist l (Size $ length l)
{-# INLINE slist #-}

infiniteSlist :: [a] -> Slist a
infiniteSlist l = Slist l Infinity
{-# INLINE infiniteSlist #-}

one :: a -> Slist a
one a = Slist [a] 1
{-# INLINE one #-}

----------------------------------------------------------------------------
-- Basic functions
----------------------------------------------------------------------------

{- | Returns the size/length of a structure as an 'Int'.
Runs in @O(1)@ time. On infinite lists returns the 'Int's 'maxBound'.

>>> size $ one 42
1
>>> size $ slist [1..3]
3
>>> size $ infiniteSlist [1..]
9223372036854775807
-}
size :: Slist a -> Int
size Slist{..} = case sSize of
    Infinity -> maxBound
    Size n   -> n
{-# INLINE size #-}

isNull :: Slist a -> Bool
isNull = (== 0) . size
{-# INLINE isNull #-}

head :: Slist a -> a
head = P.head . sList
{-# INLINE head #-}

safeHead :: Slist a -> Maybe a
safeHead Slist{..} = case sSize of
    Size 0 -> Nothing
    _      -> Just $ P.head sList
{-# INLINE safeHead #-}

last :: Slist a -> a
last = P.last . sList
{-# INLINE last #-}

safeLast :: Slist a -> Maybe a
safeLast Slist{..} = case sSize of
    Infinity -> Nothing
    Size 0   -> Nothing
    _        -> Just $ P.last sList
{-# INLINE safeLast #-}

tail :: Slist a -> Slist a
tail Slist{..} = case sSize of
    Size 0 -> mempty
    _      -> Slist (drop 1 sList) (sSize - 1)
{-# INLINE tail #-}

init :: Slist a -> Slist a
init sl@Slist{..} = case sSize of
    Infinity -> sl
    Size 0   -> mempty
    _        -> Slist (P.init sList) (sSize - 1)
{-# INLINE init #-}

uncons :: Slist a -> Maybe (a, Slist a)
uncons (Slist [] _)     = Nothing
uncons (Slist (x:xs) s) = Just (x, Slist xs $ s - 1)
{-# INLINE uncons #-}

map :: (a -> b) -> Slist a -> Slist b
map f Slist{..} = Slist (P.map f sList) sSize
{-# INLINE map #-}
