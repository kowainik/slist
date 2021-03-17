{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright:  (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

The main 'Slist' data types and instances. Provides smart constructors and a few
basic functions.
-}
module Slist.Type
    ( Slist (..)
      -- ** Smart constructors
    , slist
    , infiniteSlist
    , one
      -- * Basic functions
    , len
    , size
    , isEmpty
    , cons
    , map
    ) where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
#if ( __GLASGOW_HASKELL__ == 802 )
import Data.Semigroup (Semigroup (..))
#endif
import Prelude hiding (map)

import Slist.Size (Size (..))

import qualified Data.Foldable as F (Foldable (..))
import qualified GHC.Exts as L (IsList (..))
import qualified Prelude as P


{- | Data type that represents sized list.
Size can be both finite or infinite, it is established using
'Size' data type.
-}
data Slist a = Slist
    { sList :: [a]
    , sSize :: Size
    } deriving stock (Show, Read)

{- | Equality of sized lists is checked more efficiently
due to the fact that the check on the list sizes can be
done first for the constant time.
-}
instance (Eq a) => Eq (Slist a) where
    (Slist l1 s1) == (Slist l2 s2) = s1 == s2 && l1 == l2
    {-# INLINE (==) #-}

-- | Lexicographical comparison of the lists.
instance (Ord a) => Ord (Slist a) where
    compare (Slist l1 _) (Slist l2 _) = compare l1 l2
    {-# INLINE compare #-}

{- | List appending. Use '<>' for 'Slist' concatenation instead of
'L.++' operator that is common in ordinary list concatenations.
-}
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
        f (Slist l s) (xL, !xS) = (l ++ xL, s + xS)
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

{- | Efficient implementation of 'sum' and 'product' functions.
'length' returns 'Int's 'maxBound' on infinite lists.
-}
instance Foldable Slist where
    foldMap :: (Monoid m) => (a -> m) -> Slist a -> m
    foldMap f = foldMap f . sList
    {-# INLINE foldMap #-}

    foldr :: (a -> b -> b) -> b -> Slist a -> b
    foldr f b = foldr f b . sList
    {-# INLINE foldr #-}

    -- | Is the element in the structure?
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
    sum = F.foldl' (+) 0 . sList
    {-# INLINE sum #-}

    product :: (Num a) => Slist a -> a
    product = F.foldl' (*) 1 . sList
    {-# INLINE product #-}

    null :: Slist a -> Bool
    null = isEmpty
    {-# INLINE null #-}

    length :: Slist a -> Int
    length = len
    {-# INLINE length #-}

    toList :: Slist a -> [a]
    toList = sList
    {-# INLINE toList #-}

instance Traversable Slist where
    traverse :: (Applicative f) => (a -> f b) -> Slist a -> f (Slist b)
    traverse f (Slist l s) = (`Slist` s) <$> traverse f l
    {-# INLINE traverse #-}

instance L.IsList (Slist a) where
    type (Item (Slist a)) = a
    fromList :: [a] -> Slist a
    fromList = slist
    {-# INLINE fromList #-}

    toList :: Slist a -> [a]
    toList = sList
    {-# INLINE toList #-}

    fromListN :: Int -> [a] -> Slist a
    fromListN n l = Slist l $ Size n
    {-# INLINE fromListN #-}

{- | @O(n)@. Constructs 'Slist' from the given list.

>>> slist [1..5]
Slist {sList = [1,2,3,4,5], sSize = Size 5}

/Note:/ works with finite lists. Use 'infiniteSlist'
to construct infinite lists.
-}
slist :: [a] -> Slist a
slist l = Slist l (Size $ length l)
{-# INLINE slist #-}

{- | @O(1)@. Constructs 'Slist' from the given list.

@
>> infiniteSlist [1..]
Slist {sList = [1..], sSize = Infinity}
@

/Note:/ works with infinite lists. Use 'slist'
to construct finite lists.
-}
infiniteSlist :: [a] -> Slist a
infiniteSlist l = Slist l Infinity
{-# INLINE infiniteSlist #-}

{- | @O(1)@. Creates 'Slist' with a single element.
The size of such 'Slist' is always equals to @Size 1@.

>>> one "and only"
Slist {sList = ["and only"], sSize = Size 1}

-}
one :: a -> Slist a
one a = Slist [a] 1
{-# INLINE one #-}

----------------------------------------------------------------------------
-- Basic functions
----------------------------------------------------------------------------


{- | @O(1)@. Returns the length of a structure as an 'Int'.
On infinite lists returns the 'Int's 'maxBound'.

>>> len $ one 42
1
>>> len $ slist [1..3]
3
>>> len $ infiniteSlist [1..]
9223372036854775807
-}
len :: Slist a -> Int
len Slist{..} = case sSize of
    Infinity -> maxBound
    Size n   -> n
{-# INLINE len #-}

{- | @O(1)@. Returns the 'Size' of the slist.

>>> size $ slist "Hello World!"
Size 12
>>> size $ infiniteSlist [1..]
Infinity
-}
size :: Slist a -> Size
size = sSize
{-# INLINE size #-}

{- | @O(1)@. Checks if 'Slist' is empty

>>> isEmpty mempty
True
>>> isEmpty $ slist []
True
>>> isEmpty $ slist "Not Empty"
False
-}
isEmpty :: Slist a -> Bool
isEmpty = (== 0) . size
{-# INLINE isEmpty #-}

{- | @O(1)@. 'cons' is 'Slist' analogue to ':' for lists.
It adds the given element to the beginning of the list.

The following property is preserved:

@
  'size' ('cons' x xs) == 'size' xs + 1
@

Examples:

>>> cons 'a' $ one 'b'
Slist {sList = "ab", sSize = Size 2}

@
>> __'cons' 0 $ 'infiniteSlist' [1..]__
Slist {sList = [0..], sSize = 'Infinity'}
@
-}
cons :: a -> Slist a -> Slist a
cons x (Slist xs s) = Slist (x:xs) $ s + 1
{-# INLINE cons #-}


{- | @O(n)@. Applies the given function to each element of the slist.

> map f (slist [x1, x2, ..., xn])     == slist [f x1, f x2, ..., f xn]
> map f (infiniteSlist [x1, x2, ...]) == infiniteSlist [f x1, f x2, ...]

-}
map :: (a -> b) -> Slist a -> Slist b
map f Slist{..} = Slist (P.map f sList) sSize
{-# INLINE map #-}
