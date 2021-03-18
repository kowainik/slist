{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Use mconcat" -}

{- |
Copyright:  (c) 2019-2020 Veronika Romashkina
            (c) 2020-2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

This module introduces sized list data type — 'Slist'. The data type
has the following shape:

@
__data__ 'Slist' a = Slist
    { sList :: [a]
    , sSize :: 'Size'
    }
@

As you can see along with the familiar list, it contains 'Size' field that
represents the size of the structure. Slists can be finite or infinite, and this
is expressed with 'Size'.

@
__data__ 'Size'
    = Size 'Int'
    | Infinity
@

This representation of the list gives some additional advantages. Getting the
length of the list is the "free" operation (runs in \( O(1) \)). This property
helps to improve the performance for a bunch of functions like 'take', 'drop',
'at', etc. But also it doesn't actually add any overhead on the existing
functions.

Also, this allows to write a number of safe functions like 'safeReverse',
'safeHead', 'safeLast', 'safeIsSuffixOf', etc.

== Comparison

Check out the comparison table between lists and slists performance.

+-------------------+--------------------+--------------------+-----------------------+-----------------------+
| Function          | list (finite)      | list (infinite)    | Slist (finite)        | Slist (infinite)      |
+===================+====================+====================+=======================+=======================+
| 'length'          | \( O(n) \)         | \</hangs/\>        |  \( O(1) \)           |   \( O(1) \)          |
+-------------------+--------------------+--------------------+-----------------------+-----------------------+
| 'safeLast'        | \( O(n) \)         | \</hangs/\>        |  \( O(n) \)           |   \( O(1) \)          |
+-------------------+--------------------+--------------------+-----------------------+-----------------------+
| 'init'            | \( O(n) \)         | \</hangs/\>        |  \( O(n) \)           |   \( O(1) \)          |
+-------------------+--------------------+--------------------+-----------------------+-----------------------+
| 'take'            | \( O(min\ i\ n) \) | \( O(i) \)         | @0 < i < n@: \(O(i)\) |              \(O(i)\) |
|                   |                    |                    | otherwise:   \(O(1)\) |                       |
+-------------------+--------------------+--------------------+-----------------------+-----------------------+
| 'at'              | \( O(min\ i\ n) \) | \( O(i) \)         | @0 < i < n@: \(O(i)\) |   \( O(i) \)          |
|                   | run-time exception | run-time exception | otherwise:   \(O(1)\) |                       |
+-------------------+--------------------+--------------------+-----------------------+-----------------------+
| 'safeStripPrefix' | \( O(m) \)         |  \( O(m) \)        |  \( O(m) \)           |   \( O(m) \)          |
|                   |                    | can hang           |                       |                       |
+-------------------+--------------------+--------------------+-----------------------+-----------------------+

== Potential usage cases

* When you ask the length of the list too frequently.
* When you need to convert to data structures that require to know the list
  size in advance for allocating an array of the elements. /Example:/ [Vector data
  structure](https://hackage.haskell.org/package/vector).
* When you need to serialised lists.
* When you need to control the behaviour depending on the finiteness of the list.
* When you need a more efficient or safe implementation of some functions.

-}

module Slist
    ( -- * Types
      Slist
    , Size
      -- ** Smart constructors
    , slist
    , infiniteSlist
    , one
    , iterate
#if ( __GLASGOW_HASKELL__ > 802 )
    , iterate'
#endif
    , repeat
    , replicate
    , cycle
    , fromRange
      -- * Basic functions
    , len
    , size
    , isEmpty
    , head
    , safeHead
    , last
    , safeLast
    , init
    , tail
    , append'
    , cons
    , cons'
    , uncons

      -- * Transformations
    , map
    , reverse
    , safeReverse
    , intersperse
    , intercalate
    , transpose
    , subsequences
    , permutations

      -- *  Reducing slists (folds)
    , concat
    , concat'
    , concatMap
    , concatMap'

      -- * Building slists
      -- ** Scans
    , scanl
    , scanl'
    , scanl1
    , scanr
    , scanr1

      -- ** Unfolding
    , unfoldr

      -- * Subslists
      -- ** Extracting
    , take
    , drop
    , splitAt
    , takeWhile
    , dropWhile
    , span
    , break
    , stripPrefix
    , safeStripPrefix
    , group
    , groupBy
    , inits
    , tails
    , chunksOf
    , listChunksOf
      -- ** Predicates
    , isPrefixOf
    , safeIsPrefixOf
    , isSuffixOf
    , safeIsSuffixOf
    , isInfixOf
    , safeIsInfixOf
    , isSubsequenceOf
    , safeIsSubsequenceOf

      -- * Searching
      -- ** Searching by equality
    , lookup
      -- ** Searching with a predicate
    , filter
    , partition
    , partitionWith
    , listPartitionWith

      -- * Indexing
    , at
    , unsafeAt
    , elemIndex
    , elemIndices
    , findIndex
    , findIndices

      -- * Zipping and unzipping
    , zip
    , zip3
    , zipWith
    , zipWith3
    , unzip
    , unzip3

      -- * Sets
      -- $sets
    , nub
    , nubBy
    , ordNub
    , delete
    , deleteBy
    , deleteFirstsBy
    , diff
    , union
    , unionBy
    , intersect
    , intersectBy

      -- * Ordered slists
    , sort
    , sortBy
    , sortOn
    , sortWith
    , insert
    , insertBy

     -- * Generic functions
    , genericLength
    , genericTake
    , genericDrop
    , genericSplitAt
    , genericAt
    , genericUnsafeAt
    , genericReplicate

         -- * Maybe
    , maybeToSlist
    , slistToMaybe
    , catMaybes
    , mapMaybe
    , slistWith

      -- * Containers
      -- ** Map
    , mapToVals
    , mapToKeys
    , mapToPairs

      -- ** Set
    , setToSlist

    ) where

import Data.Bifunctor (bimap, first, second)
import Data.Either (partitionEithers)
import Data.Foldable (foldl')
#if ( __GLASGOW_HASKELL__ == 802 )
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Exts (fromListN)
import Prelude hiding (break, concat, concatMap, cycle, drop, dropWhile, filter, head, init,
                iterate, last, lookup, map, repeat, replicate, reverse, scanl, scanl1, scanr,
                scanr1, span, splitAt, tail, take, takeWhile, unzip, unzip3, zip, zip3, zipWith,
                zipWith3)

import Slist.Containers (mapToKeys, mapToPairs, mapToVals, setToSlist)
import Slist.Maybe (catMaybes, mapMaybe, maybeToSlist, slistToMaybe, slistWith)
import Slist.Size (Size (..), sizes)
import Slist.Type (Slist (..), cons, infiniteSlist, isEmpty, len, map, one, size, slist)

import qualified Data.List as L
import qualified Data.Set as Set
import qualified GHC.Exts as Exts
import qualified Prelude as P


{- | Returns an infinite slist of repeated applications
of the given function to the start element:

> iterate f x == [x, f x, f (f x), ...]

@
>> __iterate (+1) 0__
Slist {sList = [0..], sSize = 'Infinity'}
@

>>> take 5 $ iterate ('a':) "a"
Slist {sList = ["a","aa","aaa","aaaa","aaaaa"], sSize = Size 5}

/Note:/ 'L.iterate' is lazy, potentially leading to thunk build-up if
the consumer doesn't force each iterate.
See 'iterate'' for a strict variant of this function.
-}
iterate :: (a -> a) -> a -> Slist a
iterate f = infiniteSlist . L.iterate f
{-# INLINE iterate #-}

#if ( __GLASGOW_HASKELL__ > 802 )
{- | Returns an infinite slist of repeated applications
of the given function to the start element:

> iterate' f x == [x, f x, f (f x), ...]

@
>> __iterate' (+1) 0__
Slist {sList = [0..], sSize = 'Infinity'}
@

>>> take 5 $ iterate' ('a':) "a"
Slist {sList = ["a","aa","aaa","aaaa","aaaaa"], sSize = Size 5}

'iterate'' is the strict version of 'iterate'.

It ensures that the result of each application of force to weak head normal
form before proceeding.
-}
iterate' :: (a -> a) -> a -> Slist a
iterate' f = infiniteSlist . L.iterate' f
{-# INLINE iterate' #-}
#endif

{- | @O(1)@. Creates an infinite slist with the given element
at each position.

@
>> __repeat 42__
Slist {sList = [42, 42 ..], sSize = 'Infinity'}
@

>>> take 6 $ repeat 'm'
Slist {sList = "mmmmmm", sSize = Size 6}

-}
repeat :: a -> Slist a
repeat = infiniteSlist . L.repeat
{-# INLINE repeat #-}

{- | @O(n)@. Creates a finite slist with the given value at each position.

>>> replicate 3 'o'
Slist {sList = "ooo", sSize = Size 3}
>>> replicate (-11) "hmm"
Slist {sList = [], sSize = Size 0}
-}
replicate :: Int -> a -> Slist a
replicate n x
  | n <= 0 = mempty
  | otherwise = Slist (L.replicate n x) $ Size n
{-# INLINE replicate #-}

{- | Ties a finite list into a circular one, or equivalently,
the infinite repetition of the original list.
It is the identity on infinite lists.

>>> take 23 $ cycle (slist "pam ")
Slist {sList = "pam pam pam pam pam pam", sSize = Size 23}

@
>> __cycle $ 'infiniteSlist' [1..]__
Slist {sList = [1..], sSize = 'Infinity'}
@
-}
cycle :: Slist a -> Slist a
cycle sl@(Slist _ Infinity) = sl
cycle Slist{..}             = infiniteSlist $ L.cycle sList
{-# INLINE cycle #-}

{- | @O(1)@. An slist equivalent of 'P.enumFromTo' function or @[from..to]@ notation:
creates an 'Slist' of sequentially ordered values starting at @from@ and ending at @to@ inclusively.

>>> fromRange 0 5
Slist {sList = [0,1,2,3,4,5], sSize = Size 6}
>>> fromRange 5 0
Slist {sList = [], sSize = Size 0}
>>> fromRange 0 0
Slist {sList = [0], sSize = Size 1}
>>> fromRange 'a' 'd'
Slist {sList = "abcd", sSize = Size 4}
-}
fromRange :: Enum a => a -> a -> Slist a
fromRange from to = Slist [from..to] s
  where
    s :: Size
    s = Size $ max 0 (fromEnum to - fromEnum from + 1)
{-# INLINE fromRange #-}

----------------------------------------------------------------------------
-- Basic functions
----------------------------------------------------------------------------


{- | @O(1)@. Extracts the first element of a slist.
Uses not total 'L.head' function, so use wisely.

It is recommended to use 'safeHead' instead.

>>> head $ slist "qwerty"
'q'
>>> head $ infiniteSlist [1..]
1
>>> head mempty
*** Exception: Prelude.head: empty list

-}
head :: Slist a -> a
head = P.head . sList
{-# INLINE head #-}

{- | @O(1)@. Extracts the first element of a slist if possible.

>>> safeHead $ slist "qwerty"
Just 'q'
>>> safeHead $ infiniteSlist [1..]
Just 1
>>> safeHead mempty
Nothing
-}
safeHead :: Slist a -> Maybe a
safeHead Slist{..} = case sSize of
    Size 0 -> Nothing
    _      -> Just $ P.head sList
{-# INLINE safeHead #-}

{- | @O(n)@. Extracts the last element of a list.
Uses not total 'L.last' function, so use wisely.

It is recommended to use 'safeLast' instead

>>> last $ slist "qwerty"
'y'
>>> last mempty
*** Exception: Prelude.last: empty list

@
>> last $ infiniteSlist [1..]
\</hangs/\>
@
-}
last :: Slist a -> a
last = P.last . sList
{-# INLINE last #-}

{- | @O(n)@. Extracts the last element of a list if possible.

>>> safeLast $ slist "qwerty"
Just 'y'
>>> safeLast mempty
Nothing
>>> safeLast $ infiniteSlist [1..]
Nothing
-}
safeLast :: Slist a -> Maybe a
safeLast Slist{..} = case sSize of
    Infinity -> Nothing
    Size 0   -> Nothing
    _        -> Just $ P.last sList
{-# INLINE safeLast #-}

{- | @O(1)@. Returns a slist with all the elements after
the head of a given slist.

>>> tail mempty
Slist {sList = [], sSize = Size 0}
>>> tail $ slist "Hello"
Slist {sList = "ello", sSize = Size 4}

@
>> __tail $ 'infiniteSlist' [0..]__
Slist {sList = [1..], sSize = 'Infinity'}
@
-}
tail :: Slist a -> Slist a
tail Slist{..} = case sSize of
    Size 0 -> mempty
    _      -> Slist (P.drop 1 sList) (sSize - 1)
{-# INLINE tail #-}

{- | @O(n)@. Return all the elements of a list except the last one.

>>> init mempty
Slist {sList = [], sSize = Size 0}
>>> init $ slist "Hello"
Slist {sList = "Hell", sSize = Size 4}

@
>> __init $ 'infiniteSlist' [0..]__
Slist {sList = [0..], sSize = 'Infinity'}
@
-}
init :: Slist a -> Slist a
init sl@Slist{..} = case sSize of
    Infinity -> sl
    Size 0   -> mempty
    _        -> Slist (P.init sList) (sSize - 1)
{-# INLINE init #-}

{- | Strict version of the 'Slist' appending operator '<>'.

@since x.x.x.x
-}
append' :: Slist a -> Slist a -> Slist a
append' sl1 sl2
    | sSize sl1 == 0 = sl2
    | sSize sl2 == 0 = sl1
    | otherwise = let !newSize = sSize sl1 + sSize sl2 in Slist
        { sList = sList sl1 <> sList sl2
        , sSize = newSize
        }

{- | @O(1)@. Strict version of the 'cons' function
(in terms of the size evaluation).

The following property is preserved:

@
  'size' ('cons'' x xs) == 'size' xs + 1
@

Examples:

>>> cons' 'a' $ one 'b'
Slist {sList = "ab", sSize = Size 2}

@
>> __cons' 0 $ 'infiniteSlist' [1..]__
Slist {sList = [0..], sSize = 'Infinity'}
@
-}
cons' :: a -> Slist a -> Slist a
cons' x (Slist xs !s) = let !newSize = s + 1 in Slist (x:xs) newSize
{-# INLINE cons' #-}

{- | @O(1)@. Decomposes a slist into its head and tail.
If the slist is empty, returns 'Nothing'.

>>> uncons mempty
Nothing
>>> uncons $ one 'a'
Just ('a',Slist {sList = "", sSize = Size 0})

@
>> __uncons $ 'infiniteSlist' [0..]__
Just (0, Slist {sList = [1..], sSize = 'Infinity'})
@
-}
uncons :: Slist a -> Maybe (a, Slist a)
uncons (Slist [] _)     = Nothing
uncons (Slist (x:xs) s) = Just (x, Slist xs $ s - 1)
{-# INLINE uncons #-}

----------------------------------------------------------------------------
-- Transformations
----------------------------------------------------------------------------


{- | @O(n)@. Returns the elements of the slist in reverse order.

>>> reverse $ slist "Hello"
Slist {sList = "olleH", sSize = Size 5}
>>> reverse $ slist "wow"
Slist {sList = "wow", sSize = Size 3}

/Note:/ 'reverse' slist can not be calculated on infinite slists.

@
>> __reverse $ 'infiniteSlist' [1..]__
\</hangs/\>
@

Use 'safeReverse' to not hang on infinite slists.
-}
reverse :: Slist a -> Slist a
reverse Slist{..} = Slist (L.reverse sList) sSize
{-# INLINE reverse #-}

{- | @O(n)@. Returns the elements of the slist in reverse order.
On infinite slists returns the initial slist.

>>> safeReverse $ slist "Hello"
Slist {sList = "olleH", sSize = Size 5}

@
>> __reverse $ 'infiniteSlist' [1..]__
Slist {sList = [1..], sSize = 'Infinity'}
@
-}
safeReverse :: Slist a -> Slist a
safeReverse sl@(Slist _ Infinity) = sl
safeReverse sl                    = reverse sl
{-# INLINE safeReverse #-}

{- | @O(n)@. Takes an element and a list and intersperses
that element between the elements of the list.

>>> intersperse ',' $ slist "abcd"
Slist {sList = "a,b,c,d", sSize = Size 7}
>>> intersperse '!' mempty
Slist {sList = "", sSize = Size 0}

@
>> __intersperse 0 $ 'infiniteSlist' [1,1..]__
Slist {sList = [1,0,1,0..], sSize = 'Infinity'}
@
-}
intersperse :: a -> Slist a -> Slist a
intersperse _ sl@(Slist _ (Size 0)) = sl
intersperse a Slist{..}             = Slist (L.intersperse a sList) (2 * sSize - 1)
{-# INLINE intersperse #-}

{- | @O(n)@. Inserts the given slist in between the slists and concatenates the result.

> intercalate x xs = concat (intersperse x xs)

>>> intercalate (slist ", ") $ slist [slist "Lorem", slist "ipsum", slist "dolor"]
Slist {sList = "Lorem, ipsum, dolor", sSize = Size 19}

-}
intercalate :: Slist a -> Slist (Slist a) -> Slist a
intercalate x = foldr (<>) mempty . intersperse x
{-# INLINE intercalate #-}

{- | @O(n * m)@. Transposes the rows and columns of the slist.

>>> transpose $ slist [slist [1,2]]
Slist {sList = [Slist {sList = [1], sSize = Size 1},Slist {sList = [2], sSize = Size 1}], sSize = Size 2}

@
>> __transpose $ slist [slist [1,2,3], slist [4,5,6]]__
Slist { sList =
          [ Slist {sList = [1,4], sSize = Size 2}
          , Slist {sList = [2,5], sSize = Size 2}
          , Slist {sList = [3,6], sSize = Size 2}
          ]
      , sSize = Size 3
      }
@

If some of the rows are shorter than the following rows, their elements are skipped:

>>> transpose $ slist [slist [10,11], slist [20], mempty]
Slist {sList = [Slist {sList = [10,20], sSize = Size 2},Slist {sList = [11], sSize = Size 1}], sSize = Size 2}

If some of the rows is an infinite slist, then the resulting slist is going to be infinite.
-}
transpose :: Slist (Slist a) -> Slist (Slist a)
transpose (Slist l _) = Slist
    { sList = P.map slist $ L.transpose $ P.map sList l
    , sSize = maximum $ P.map sSize l
    }
{-# INLINE transpose #-}

{- | @O(2 ^ n)@. Returns the list of all subsequences of the argument.

>>> subsequences mempty
Slist {sList = [Slist {sList = [], sSize = Size 0}], sSize = Size 1}
>>> subsequences $ slist "ab"
Slist {sList = [Slist {sList = "", sSize = Size 0},Slist {sList = "a", sSize = Size 1},Slist {sList = "b", sSize = Size 1},Slist {sList = "ab", sSize = Size 2}], sSize = Size 4}
>>> take 4 $ subsequences $ infiniteSlist [1..]
Slist {sList = [Slist {sList = [], sSize = Size 0},Slist {sList = [1], sSize = Size 1},Slist {sList = [2], sSize = Size 1},Slist {sList = [1,2], sSize = Size 2}], sSize = Size 4}

-}
subsequences :: Slist a -> Slist (Slist a)
subsequences Slist{..} = Slist
    { sList = P.map slist $ L.subsequences sList
    , sSize = newSize sSize
    }
  where
    newSize :: Size -> Size
    newSize Infinity = Infinity
    newSize (Size n) = Size $ 2 ^ toInteger n
{-# INLINE subsequences #-}

{- | @O(n!)@. Returns the list of all permutations of the argument.

>>> permutations mempty
Slist {sList = [Slist {sList = [], sSize = Size 0}], sSize = Size 1}
>>> permutations $ slist "abc"
Slist {sList = [Slist {sList = "abc", sSize = Size 3},Slist {sList = "bac", sSize = Size 3},Slist {sList = "cba", sSize = Size 3},Slist {sList = "bca", sSize = Size 3},Slist {sList = "cab", sSize = Size 3},Slist {sList = "acb", sSize = Size 3}], sSize = Size 6}

-}
permutations :: Slist a -> Slist (Slist a)
permutations (Slist l s) = Slist
    { sList = P.map (`Slist` s) $ L.permutations l
    , sSize = fact s
    }
  where
    fact :: Size -> Size
    fact Infinity = Infinity
    fact (Size n) = Size $ go 1 n

    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc n = go (acc * n) (n - 1)
{-# INLINE permutations #-}

----------------------------------------------------------------------------
-- Reducing slists (folds)
----------------------------------------------------------------------------

{- | \( O(\sum n_i) \) The concatenation of all the elements of a container of slists.

>>> concat [slist [1,2], slist [3..5], slist [6..10]]
Slist {sList = [1,2,3,4,5,6,7,8,9,10], sSize = Size 10}

@
>> __concat $ slist [slist [1,2], 'infiniteSlist' [3..]]__
Slist {sList = [1..], sSize = 'Infinity'}
@
-}
concat :: Foldable t => t (Slist a) -> Slist a
concat = foldr (<>) mempty
{-# INLINE concat #-}

{- | \( O(\sum n_i) \) The concatenation of all the elements of a container of slists.

The strict version of 'concat'.

>>> concat' [slist [1,2], slist [3..5], slist [6..10]]
Slist {sList = [1,2,3,4,5,6,7,8,9,10], sSize = Size 10}

@
>> __concat' $ slist [slist [1,2], 'infiniteSlist' [3..]]__
Slist {sList = [1..], sSize = 'Infinity'}
@

@since x.x.x.x
-}
concat' :: Foldable t => t (Slist a) -> Slist a
concat' = foldl' append' mempty
{-# INLINE concat' #-}

{- | Maps a function over all the elements of a container
and concatenates the resulting slists.

>>> concatMap one "abc"
Slist {sList = "abc", sSize = Size 3}
-}
concatMap :: Foldable t => (a -> Slist b) -> t a -> Slist b
concatMap = foldMap
{-# INLINE concatMap #-}

{- | Maps a function over all the elements of a container and concatenates the
resulting slists.

Strict version of 'concatMap'.

>>> concatMap' one "abc"
Slist {sList = "abc", sSize = Size 3}

@since x.x.x.x
-}
concatMap' :: Foldable t => (a -> Slist b) -> t a -> Slist b
concatMap' f = foldl' (\acc x -> acc `append'` f x) mempty
{-# INLINE concatMap' #-}

----------------------------------------------------------------------------
-- Building lists
----------------------------------------------------------------------------

{- | @O(n)@. Similar to 'foldl', but returns a slist of successive
reduced values from the left:

> scanl f z $ slist [x1, x2, ...] == slist [z, z `f` x1, (z `f` x1) `f` x2, ...]

Note that

> last (scanl f z xs) == foldl f z xs.

This peculiar arrangement is necessary to prevent scanl being rewritten in
its own right-hand side.

>>> scanl (+) 0 $ slist [1..10]
Slist {sList = [0,1,3,6,10,15,21,28,36,45,55], sSize = Size 11}

-}
scanl :: (b -> a -> b) -> b -> Slist a -> Slist b
scanl f b Slist{..} = Slist (L.scanl f b sList) (sSize + 1)
{-# INLINE scanl #-}

-- | @O(n)@. A strictly accumulating version of 'scanl'
scanl' :: (b -> a -> b) -> b -> Slist a -> Slist b
scanl' f b Slist{..} = Slist (L.scanl' f b sList) (sSize + 1)
{-# INLINE scanl' #-}

{- | @O(n)@. 'scanl1' is a variant of 'scanl' that has no starting value argument:

> scanl1 f $ slist [x1, x2, ...] == slist [x1, x1 `f` x2, ...]
-}
scanl1 :: (a -> a -> a) -> Slist a -> Slist a
scanl1 f Slist{..} = Slist (L.scanl1 f sList) sSize
{-# INLINE scanl1 #-}

{- | @O(n)@. The right-to-left dual of 'scanl'.

Note that

> head (scanr f z xs) == foldr f z xs.

>>> scanr (+) 0 $ slist [1..10]
Slist {sList = [55,54,52,49,45,40,34,27,19,10,0], sSize = Size 11}

-}
scanr :: (a -> b -> b) -> b -> Slist a -> Slist b
scanr f b Slist{..} = Slist (L.scanr f b sList) (sSize + 1)
{-# INLINE scanr #-}

-- | A variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> Slist a -> Slist a
scanr1 f Slist{..} = Slist (L.scanr1 f sList) sSize
{-# INLINE scanr1 #-}

{- | @O(n)@. A \`dual\' to 'foldr': while 'foldr'
reduces a list to a summary value, 'unfoldr' builds a list from
a seed value.  The function takes the element and returns 'Nothing'
if it is done producing the list or returns 'Just' @(a,b)@, in which
case, @a@ is a prepended to the list and @b@ is used as the next
element in a recursive call.

In some cases, 'unfoldr' can undo a 'foldr' operation:

> unfoldr f' (foldr f z xs) == xs

if the following holds:

> f' (f x y) = Just (x,y)
> f' z       = Nothing

A simple use of unfoldr:

>>> unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
Slist {sList = [10,9,8,7,6,5,4,3,2,1], sSize = Size 10}
-}
unfoldr :: forall a b . (b -> Maybe (a, b)) -> b -> Slist a
unfoldr f def = let (s, l) = go def in Slist l $ Size s
  where
    go :: b -> (Int, [a])
    go b = case f b of
        Just (a, newB) -> bimap (+ 1) (a:) $ go newB
        Nothing        -> (0, [])
{-# INLINE unfoldr #-}

----------------------------------------------------------------------------
-- Sublists
----------------------------------------------------------------------------

{- | @O(i) | i < n@ and @O(1) | otherwise@.

Returns the prefix of the slist of the given length.
If the given @i@ is non-positive then the empty structure is returned.
If @i@ is exceeds the length of the structure the initial slist is returned.

>>> take 5 $ slist "Hello world!"
Slist {sList = "Hello", sSize = Size 5}
>>> take 20 $ slist "small"
Slist {sList = "small", sSize = Size 5}
>>> take 0 $ slist "none"
Slist {sList = "", sSize = Size 0}
>>> take (-11) $ slist "hmm"
Slist {sList = "", sSize = Size 0}
>>> take 4 $ infiniteSlist [1..]
Slist {sList = [1,2,3,4], sSize = Size 4}
-}
take :: Int -> Slist a -> Slist a
take i sl@Slist{..}
    | Size i >= sSize = sl
    | i <= 0 = mempty
    | otherwise = Slist
        { sList = P.take i sList
        , sSize = Size i
        }
{-# INLINE take #-}

{- | @O(i) | i < n@ and @O(1) | otherwise@.

Returns the suffix of the slist after the first @i@ elements.
If @i@ exceeds the length of the slist then the empty structure is returned.
If @i@ is non-positive the initial structure is returned.

>>> drop 6 $ slist "Hello World"
Slist {sList = "World", sSize = Size 5}
>>> drop 42 $ slist "oops!"
Slist {sList = "", sSize = Size 0}
>>> drop 0 $ slist "Hello World!"
Slist {sList = "Hello World!", sSize = Size 12}
>>> drop (-4) $ one 42
Slist {sList = [42], sSize = Size 1}

@
>> __drop 5 $ 'infiniteSlist' [1..]__
Slist {sList = [6..], sSize = 'Infinity'}
@

-}
drop :: Int -> Slist a -> Slist a
drop i sl@Slist{..}
    | i <= 0 = sl
    | Size i >= sSize = mempty
    | otherwise = Slist
        { sList = P.drop i sList
        , sSize = sSize - Size i
        }
{-# INLINE drop #-}

{- | @O(i) | i < n@ and @O(1) | otherwise@.

Returns a tuple where the first element is the prefix
of the given length and the second element is the remainder
of the slist.

>>> splitAt 5 $ slist "Hello World!"
(Slist {sList = "Hello", sSize = Size 5},Slist {sList = " World!", sSize = Size 7})
>>> splitAt 0 $ slist "abc"
(Slist {sList = "", sSize = Size 0},Slist {sList = "abc", sSize = Size 3})
>>> splitAt 4 $ slist "abc"
(Slist {sList = "abc", sSize = Size 3},Slist {sList = "", sSize = Size 0})
>>>splitAt (-42) $ slist "??"
(Slist {sList = "", sSize = Size 0},Slist {sList = "??", sSize = Size 2})

@
>> __splitAt 2 $ 'infiniteSlist' [1..]__
(Slist {sList = [1,2], sSize = 'Size' 2}, Slist {sList = [3..], sSize = 'Infinity'})
@
-}
splitAt :: Int -> Slist a -> (Slist a, Slist a)
splitAt i sl@Slist{..}
    | i <= 0 = (mempty, sl)
    | Size i >= sSize = (sl, mempty)
    | otherwise =
        let (l1, l2) = P.splitAt i sList
            s2 = sSize - Size i
        in (Slist l1 $ Size i, Slist l2 s2)
{-# INLINE splitAt #-}

{- | @O(n)@. Returns the longest prefix (possibly empty)
of elements that satisfy the given predicate.

>>> takeWhile (<3) $ slist [1..10]
Slist {sList = [1,2], sSize = Size 2}
>>> takeWhile (<3) $ infiniteSlist [1..]
Slist {sList = [1,2], sSize = Size 2}
>>> takeWhile (<=10) $ slist [1..10]
Slist {sList = [1,2,3,4,5,6,7,8,9,10], sSize = Size 10}
>>> takeWhile (<0) $ slist [1..10]
Slist {sList = [], sSize = Size 0}

-}
takeWhile :: forall a . (a -> Bool) -> Slist a -> Slist a
takeWhile p Slist{..} = let (s, l) = go 0 sList in
    Slist l $ Size s
  where
    go :: Int -> [a] -> (Int, [a])
    go !n [] = (n, [])
    go !n (x:xs) =
        if p x
        then let (i, l) = go (n + 1) xs in (i, x:l)
        else (n, [])
{-# INLINE takeWhile #-}

{- | @O(n)@. Returns the suffix (possibly empty) of the remaining
elements after dropping elements that satisfy the given predicate.

>>> dropWhile (<3) $ slist [1..10]
Slist {sList = [3,4,5,6,7,8,9,10], sSize = Size 8}
>>> dropWhile (<=10) $ slist [1..10]
Slist {sList = [], sSize = Size 0}
>>> dropWhile (<0) $ slist [1..10]
Slist {sList = [1,2,3,4,5,6,7,8,9,10], sSize = Size 10}
>>> take 5 $ dropWhile (<3) $ infiniteSlist [1..]
Slist {sList = [3,4,5,6,7], sSize = Size 5}

@
>> __dropWhile (< 5) $ 'infiniteSlist' [1..]__
Slist {sList = [5,6..], sSize = 'Infinity'}
@
-}
dropWhile :: forall a . (a -> Bool) -> Slist a -> Slist a
dropWhile p (Slist l Infinity) = Slist (P.dropWhile p l) Infinity
dropWhile p Slist{..} = let (s, l) = go 0 sList in
    Slist l (sSize - Size s)
  where
    go :: Int -> [a] -> (Int, [a])
    go !n [] = (n, [])
    go !n (x:xs) =
        if p x
        then go (n + 1) xs
        else (n, x:xs)
{-# INLINE dropWhile #-}

{- | @O(n)@. Returns a tuple where first element is longest prefix (possibly empty)
of the slist of elements that satisfy the given predicate
and second element is the remainder of the list.

>>> span (<3) $ slist [1,2,3,4,1,2,3,4]
(Slist {sList = [1,2], sSize = Size 2},Slist {sList = [3,4,1,2,3,4], sSize = Size 6})
>>> span (<=10) $ slist [1..3]
(Slist {sList = [1,2,3], sSize = Size 3},Slist {sList = [], sSize = Size 0})
>>> span (<0) $ slist [1..3]
(Slist {sList = [], sSize = Size 0},Slist {sList = [1,2,3], sSize = Size 3})

@
>> __span (<3) $ 'infiniteSlist' [1..]__
(Slist {sList = [1,2], sSize = Size 2}, Slist {sList = [3..], sSize = 'Infinity'})
@
-}
span :: forall a . (a -> Bool) -> Slist a -> (Slist a, Slist a)
span p Slist{..} = let (s, l, r) = go 0 sList in
    ( Slist l $ Size s
    , Slist r (sSize - Size s)
    )
  where
    go :: Int -> [a] -> (Int, [a], [a])
    go !n [] = (n, [], [])
    go !n (x:xs) =
        if p x
        then let (s, l, r) = go (n + 1) xs in (s, x:l, r)
        else (n, [], x:xs)
{-# INLINE span #-}

{- | @O(n)@.  Returns a tuple where first element is longest prefix (possibly empty)
of the slist of elements that /do not/ satisfy the given predicate
and second element is the remainder of the list.

@
> break p = 'span' ('not' . p)
@
-}
break :: (a -> Bool) -> Slist a -> (Slist a, Slist a)
break p = span (not . p)
{-# INLINE break #-}

{- | @O(n)@. Splits a 'Slist' into components of the given length. The last
element may be shorter than the other chunks, depending on the length of the
input.

>>> chunksOf 3 $ slist [0..7]
Slist {sList = [Slist {sList = [0,1,2], sSize = Size 3},Slist {sList = [3,4,5], sSize = Size 3},Slist {sList = [6,7], sSize = Size 2}], sSize = Size 3}
>>> chunksOf 0 $ slist [0..10]
Slist {sList = [], sSize = Size 0}
>>> chunksOf (-13) $ slist [0..10]
Slist {sList = [], sSize = Size 0}
>>> chunksOf 100 $ slist [1,2,3]
Slist {sList = [Slist {sList = [1,2,3], sSize = Size 3}], sSize = Size 1}

>>> take 2 $ chunksOf 3 $ infiniteSlist [1..]
Slist {sList = [Slist {sList = [1,2,3], sSize = Size 3},Slist {sList = [4,5,6], sSize = Size 3}], sSize = Size 2}

@since x.x.x.x
-}
chunksOf :: Int -> Slist a -> Slist (Slist a)
chunksOf i sl@Slist{..}
    | i <= 0 = mempty
    | sSize == Infinity = Slist (P.map (fromListN i) $ listChunksOf i sList) Infinity
    | otherwise = go sl
  where
    go :: Slist a -> Slist (Slist a)
    go x@(Slist _ s)
        | Size i >= s = one x
        | otherwise =
            let (chunk, rest) = splitAt i x
            in cons chunk $ go rest
{-# INLINE chunksOf #-}

{- | @O(n)@. Splits a list into components of the given length. The last
element may be shorter than the other chunks, depending on the length of the
input.

>>> listChunksOf 3 [0..7]
[[0,1,2],[3,4,5],[6,7]]
>>> listChunksOf 0 [0..10]
[]
>>> listChunksOf (-13) [0..10]
[]
>>> listChunksOf 100 [1,2,3]
[[1,2,3]]

>>> P.take 2 $ listChunksOf 3 [1..]
[[1,2,3],[4,5,6]]

@since x.x.x.x
-}
listChunksOf :: Int -> [a] -> [[a]]
listChunksOf i l
    | i <= 0 = mempty
    | otherwise = go l
  where
    go :: [a] -> [[a]]
    go [] = []
    go x =
        let (chunk, rest) = P.splitAt i x
        in chunk : go rest
{-# INLINE listChunksOf #-}

{- | @O(m)@. Drops the given prefix from a list.
It returns 'Nothing' if the slist did not start with the given prefix,
or 'Just' the slist after the prefix, if it does.

>>> stripPrefix (slist "foo") (slist "foobar")
Just (Slist {sList = "bar", sSize = Size 3})
>>> stripPrefix (slist "foo") (slist "foo")
Just (Slist {sList = "", sSize = Size 0})
>>> stripPrefix (slist "foo") (slist "barfoo")
Nothing
>>> stripPrefix mempty  (slist "foo")
Just (Slist {sList = "foo", sSize = Size 3})
>>> stripPrefix (infiniteSlist [0..]) (infiniteSlist [1..])
Nothing

/Note:/ this function could hang on the infinite slists.

@
>> __stripPrefix (infiniteSlist [1..]) (infiniteSlist [1..])__
\</hangs/\>
@

Use 'safeStripPrefix' instead.
-}
stripPrefix :: Eq a => Slist a -> Slist a -> Maybe (Slist a)
stripPrefix (Slist l1 s1) f@(Slist l2 s2)
    | s1 == Size 0 = Just f
    | s1 > s2 = Nothing
    | otherwise = (\l -> Slist l $ s2 - s1) <$> L.stripPrefix l1 l2
{-# INLINE stripPrefix #-}

{- | Similar to 'stripPrefix', but never hangs on infinite lists
and returns 'Nothing' in that case.

>>> safeStripPrefix (infiniteSlist [1..]) (infiniteSlist [1..])
Nothing
>>> safeStripPrefix (infiniteSlist [0..]) (infiniteSlist [1..])
Nothing

-}
safeStripPrefix :: Eq a => Slist a -> Slist a -> Maybe (Slist a)
safeStripPrefix (Slist _ Infinity) (Slist _ Infinity) = Nothing
safeStripPrefix sl1 sl2                               = stripPrefix sl1 sl2
{-# INLINE safeStripPrefix #-}

{- | @O(n)@. Takes a slist and returns a slist of slists such
that the concatenation of the result is equal to the argument.
Moreover, each sublist in the result contains only equal elements.

It is a special case of 'groupBy', which allows
to supply their own equality test.

>>> group $ slist "Mississippi"
Slist {sList = [Slist {sList = "M", sSize = Size 1},Slist {sList = "i", sSize = Size 1},Slist {sList = "ss", sSize = Size 2},Slist {sList = "i", sSize = Size 1},Slist {sList = "ss", sSize = Size 2},Slist {sList = "i", sSize = Size 1},Slist {sList = "pp", sSize = Size 2},Slist {sList = "i", sSize = Size 1}], sSize = Size 8}
>>> group mempty
Slist {sList = [], sSize = Size 0}

-}
group :: Eq a => Slist a -> Slist (Slist a)
group = groupBy (==)
{-# INLINE group #-}

{- | @O(n)@. Non-overloaded version of the 'group' function.

>>> groupBy (>) $ slist "Mississippi"
Slist {sList = [Slist {sList = "M", sSize = Size 1},Slist {sList = "i", sSize = Size 1},Slist {sList = "s", sSize = Size 1},Slist {sList = "si", sSize = Size 2},Slist {sList = "s", sSize = Size 1},Slist {sList = "sippi", sSize = Size 5}], sSize = Size 6}

-}
groupBy :: (a -> a -> Bool) -> Slist a -> Slist (Slist a)
groupBy p (Slist l Infinity) = infiniteSlist $ P.map slist $ L.groupBy p l
groupBy p Slist{..}          = slist $ P.map slist $ L.groupBy p sList
{-# INLINE groupBy #-}

{- | @O(n)@. Returns all initial segments of the argument, shortest first.

>>> inits $ slist "abc"
Slist {sList = [Slist {sList = "", sSize = Size 0},Slist {sList = "a", sSize = Size 1},Slist {sList = "ab", sSize = Size 2},Slist {sList = "abc", sSize = Size 3}], sSize = Size 4}
>>> inits mempty
Slist {sList = [Slist {sList = [], sSize = Size 0}], sSize = Size 1}

-}
inits :: Slist a -> Slist (Slist a)
inits (Slist l s) = Slist
    { sList = L.zipWith Slist (L.inits l) $ sizes s
    , sSize = s + 1
    }
{-# INLINE inits #-}

{- | @O(n)@. Returns all final segments of the argument, shortest first.

>>> tails $ slist "abc"
Slist {sList = [Slist {sList = "abc", sSize = Size 3},Slist {sList = "bc", sSize = Size 2},Slist {sList = "c", sSize = Size 1},Slist {sList = "", sSize = Size 0}], sSize = Size 4}
>>> tails mempty
Slist {sList = [Slist {sList = [], sSize = Size 0}], sSize = Size 1}

-}
tails :: Slist a -> Slist (Slist a)
tails (Slist l Infinity) = infiniteSlist $ P.map infiniteSlist (L.tails l)
tails (Slist l s@(Size n)) = Slist
    { sList = L.zipWith (\li i -> Slist li $ Size i) (L.tails l) [n, n - 1 .. 0]
    , sSize = s + 1
    }
{-# INLINE tails #-}

{- | @O(m)@.
Takes two slists and returns 'True' iff the first slist
is a prefix of the second.

>>> isPrefixOf (slist "Hello") (slist "Hello World!")
True
>>> isPrefixOf (slist "Hello World!") (slist "Hello")
False
>>> isPrefixOf mempty (slist "hey")
True

/Note:/ this function could hang on the infinite slists.

@
>> __isPrefixOf (infiniteSlist [1..]) (infiniteSlist [1..])__
\</hangs/\>
@

Use 'safeIsPrefixOf' instead.

-}
isPrefixOf :: Eq a => Slist a -> Slist a -> Bool
isPrefixOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = l1 `L.isPrefixOf` l2
{-# INLINE isPrefixOf #-}

{- | Similar to 'isPrefixOf', but never hangs on infinite lists
and returns 'False' in that case.

>>> safeIsPrefixOf (infiniteSlist [1..]) (infiniteSlist [1..])
False
>>> safeIsPrefixOf (infiniteSlist [0..]) (infiniteSlist [1..])
False
-}
safeIsPrefixOf :: Eq a => Slist a -> Slist a -> Bool
safeIsPrefixOf sl1@(Slist _ s1) sl2@(Slist _ s2)
    | s1 == Infinity && s2 == Infinity = False
    | otherwise = sl1 `isPrefixOf` sl2
{-# INLINE safeIsPrefixOf #-}

{- |
Takes two slists and returns 'True' iff the first slist
is a suffix of the second.

>>> isSuffixOf (slist "World!") (slist "Hello World!")
True
>>> isSuffixOf (slist "Hello World!") (slist "Hello")
False
>>> isSuffixOf mempty (slist "hey")
True

/Note:/ this function hangs if the second slist is infinite.

@
>> __isSuffixOf /anything/ (infiniteSlist [1..])__
\</hangs/\>
@

Use 'safeIsSuffixOf' instead.
-}
isSuffixOf :: Eq a => Slist a -> Slist a -> Bool
isSuffixOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = l1 `L.isSuffixOf` l2
{-# INLINE isSuffixOf #-}

{- | Similar to 'isSuffixOf', but never hangs on infinite lists
and returns 'False' in that case.

>>> safeIsSuffixOf (slist [1,2]) (infiniteSlist [1..])
False
>>> safeIsSuffixOf (infiniteSlist [1..]) (infiniteSlist [1..])
False
-}
safeIsSuffixOf :: Eq a => Slist a -> Slist a -> Bool
safeIsSuffixOf sl1 sl2@(Slist _ s2)
    | s2 == Infinity = False
    | otherwise = sl1 `isSuffixOf` sl2
{-# INLINE safeIsSuffixOf #-}

{- |
Takes two slists and returns 'True' iff the first slist
is contained, wholly and intact, anywhere within the second.

>>> isInfixOf (slist "ll") (slist "Hello World!")
True
>>> isInfixOf (slist " Hello") (slist "Hello")
False
>>> isInfixOf (slist "Hello World!") (slist "Hello")
False

/Note:/ this function could hang on the infinite slists.

@
>> __isPrefixOf (infiniteSlist [1..]) (infiniteSlist [1..])__
\</hangs/\>
@

Use 'safeIsInfixOf' instead.
-}
isInfixOf :: Eq a => Slist a -> Slist a -> Bool
isInfixOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = l1 `L.isInfixOf` l2
{-# INLINE isInfixOf #-}

{- | Similar to 'isInfixOf', but never hangs on infinite lists
and returns 'False' in that case.

>>> safeIsInfixOf (infiniteSlist [1..]) (infiniteSlist [1..])
False
>>> safeIsInfixOf (infiniteSlist [0..]) (infiniteSlist [1..])
False
-}
safeIsInfixOf :: Eq a => Slist a -> Slist a -> Bool
safeIsInfixOf sl1@(Slist _ s1) sl2@(Slist _ s2)
    | s1 == Infinity && s2 == Infinity = False
    | otherwise = sl1 `isInfixOf` sl2
{-# INLINE safeIsInfixOf #-}

{- |
Takes two slists and returns 'True' if all the elements
of the first slist occur, in order, in the second.
The elements do not have to occur consecutively.

@isSubsequenceOf x y@ is equivalent to @'elem' x ('subsequences' y)@.

>>> isSubsequenceOf (slist "Hll") (slist "Hello World!")
True
>>> isSubsequenceOf (slist "") (slist "Hello World!")
True
>>> isSubsequenceOf (slist "Hallo") (slist "Hello World!")
False

/Note:/ this function hangs if the second slist is infinite.

@
>> __isSuffixOf /anything/ (infiniteSlist [1..])__
\</hangs/\>
@

Use 'safeIsSuffixOf' instead.
-}
isSubsequenceOf :: Eq a => Slist a -> Slist a -> Bool
isSubsequenceOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = L.isSubsequenceOf l1 l2
{-# INLINE isSubsequenceOf #-}

{- | Similar to 'isSubsequenceOf', but never hangs on infinite lists
and returns 'False' in that case.

>>> safeIsSubsequenceOf (infiniteSlist [1..]) (infiniteSlist [1..])
False
>>> safeIsSubsequenceOf (infiniteSlist [0..]) (infiniteSlist [1..])
False
-}
safeIsSubsequenceOf :: Eq a => Slist a -> Slist a -> Bool
safeIsSubsequenceOf sl1@(Slist _ s1) sl2@(Slist _ s2)
    | s1 == Infinity && s2 == Infinity = False
    | otherwise = isSubsequenceOf sl1 sl2
{-# INLINE safeIsSubsequenceOf #-}

----------------------------------------------------------------------------
-- Searching
----------------------------------------------------------------------------

{- | @O(n)@.
Looks up by the given key in the slist of key-value pairs.

>>> lookup 42 $ slist $ [(1, "one"), (2, "two")]
Nothing
>>> lookup 42 $ slist $ [(1, "one"), (2, "two"), (42, "life, the universe and everything")]
Just "life, the universe and everything"
>>> lookup 1 $ zip (infiniteSlist  [1..]) (infiniteSlist [0..])
Just 0
-}
lookup :: Eq a => a -> Slist (a, b) -> Maybe b
lookup a = L.lookup a . sList
{-# INLINE lookup #-}

{- | @O(n)@.
Returns the slist of the elements that satisfy the given predicate.

>>> filter (<3) $ slist [1..5]
Slist {sList = [1,2], sSize = Size 2}
>>> take 5 $ filter odd $ infiniteSlist [1..]
Slist {sList = [1,3,5,7,9], sSize = Size 5}
-}
filter :: forall a . (a -> Bool) -> Slist a -> Slist a
filter p (Slist l Infinity) = infiniteSlist $ L.filter p l
filter p Slist{..} = let (newS, newL) = go 0 sList in
    Slist newL (Size newS)
  where
    go :: Int -> [a] -> (Int, [a])
    go !n [] = (n, [])
    go n (x:xs) =
        if p x
        then second (x:) $ go (n + 1) xs
        else go n xs
{-# INLINE filter #-}

{- | @O(n)@.
Returns the pair of slists of elements which do and do not satisfy
the given predicate.

>>> partition (<3) $ slist [1..5]
(Slist {sList = [1,2], sSize = Size 2},Slist {sList = [3,4,5], sSize = Size 3})
-}
partition :: forall a . (a -> Bool) -> Slist a -> (Slist a, Slist a)
partition p (Slist l Infinity) = bimap infiniteSlist infiniteSlist $ L.partition p l
partition p Slist{..} = let (s1, l1, l2) = go 0 sList in
    (Slist l1 $ Size s1, Slist l2 $ sSize - Size s1)
  where
    go :: Int -> [a] -> (Int, [a], [a])
    go !n [] = (n, [], [])
    go n (x:xs) =
        if p x
        then first (x:) $ go (n + 1) xs
        else second (x:) $ go n xs
{-# INLINE partition #-}

{- | @O(n)@.
Returns the pair of slists of elements resulting to 'Left' and resulting to
'Right' by applying the given function.

>>> onEven x = if even x then Right x else Left ("Oops: " ++ show x)
>>> partitionWith onEven $ slist [1..5]
(Slist {sList = ["Oops: 1","Oops: 3","Oops: 5"], sSize = Size 3},Slist {sList = [2,4], sSize = Size 2})

@since x.x.x.x
-}
partitionWith :: forall a b c . (a -> Either b c) -> Slist a -> (Slist b, Slist c)
partitionWith f (Slist l Infinity) = bimap infiniteSlist infiniteSlist $ listPartitionWith f l
partitionWith f Slist{..} = let (s1, l1, l2) = go 0 sList in
    (Slist l1 $ Size s1, Slist l2 $ sSize - Size s1)
  where
    go :: Int -> [a] -> (Int, [b], [c])
    go !n [] = (n, [], [])
    go n (x:xs) = case f x of
        Left  b -> first  (b:) $ go (n + 1) xs
        Right c -> second (c:) $ go n xs
{-# INLINE partitionWith #-}

{- | @O(n)@.
Returns the pair of lists of elements resulting to 'Left' and resulting to
'Right' by applying the given function.


>>> onEven x = if even x then Right x else Left ("Oops: " ++ show x)
>>> listPartitionWith onEven [1..5]
(["Oops: 1","Oops: 3","Oops: 5"],[2,4])

@since x.x.x.x
-}
listPartitionWith :: forall a b c . (a -> Either b c) -> [a] -> ([b], [c])
listPartitionWith f = partitionEithers . L.map f
{-# INLINE listPartitionWith #-}

----------------------------------------------------------------------------
-- Indexing
----------------------------------------------------------------------------

{- | @O(i) | i < n@ and @O(1) | otherwise@.

Returns the element of the slist at the given position.
If the @i@ exceeds the length of the slist the function returns 'Nothing'.

>>> let sl = slist [1..10]
>>> at 0 sl
Just 1
>>> at (-1) sl
Nothing
>>> at 11 sl
Nothing
>>> at 9 sl
Just 10
-}
at :: Int -> Slist a -> Maybe a
at n Slist{..}
    | n < 0 || Size n >= sSize = Nothing
    | otherwise = Just $ sList L.!! n
{-# INLINE at #-}

{- | @O(min i n)@.
Unsafe version of the 'at' function.
If the element on the given position does not exist
it throws the exception at run-time.

>>> let sl = slist [1..10]
>>> unsafeAt 0 sl
1
>>> unsafeAt (-1) sl
*** Exception: Prelude.!!: negative index
>>> unsafeAt 11 sl
*** Exception: Prelude.!!: index too large
>>> unsafeAt 9 sl
10
-}
unsafeAt :: Int -> Slist a -> a
unsafeAt n Slist{..} = sList L.!! n
{-# INLINE unsafeAt #-}

{- | @O(n)@.
Returns the index of the first element in the given slist which is equal
(by '==') to the query element, or 'Nothing' if there is no such element.

>>> elemIndex 5 $ slist [1..10]
Just 4
>>> elemIndex 0 $ slist [1..10]
Nothing
-}
elemIndex :: Eq a => a -> Slist a -> Maybe Int
elemIndex a = L.elemIndex a . sList
{-# INLINE elemIndex #-}

{- | @O(n)@.
Extends 'elemIndex', by returning the indices of all elements equal
to the query element, in ascending order.

>>> elemIndices 1 $ slist [1,1,1,2,2,4,5,1,9,1]
Slist {sList = [0,1,2,7,9], sSize = Size 5}
>>> take 5 $ elemIndices 1 $ repeat 1
Slist {sList = [0,1,2,3,4], sSize = Size 5}
-}
elemIndices :: Eq a => a -> Slist a -> Slist Int
elemIndices a = findIndices (a ==)
{-# INLINE elemIndices #-}

{- | @O(n)@.
Returns the index of the first element in the slist satisfying
the given predicate, or 'Nothing' if there is no such element.

>>> findIndex (>3) $ slist [1..5]
Just 3
>>> findIndex (<0) $ slist [1..5]
Nothing
-}
findIndex :: (a -> Bool) -> Slist a -> Maybe Int
findIndex p = L.findIndex p . sList
{-# INLINE findIndex #-}

{- | @O(n)@.
Extends 'findIndex', by returning the indices of all elements
satisfying the given predicate, in ascending order.

>>> findIndices (<3) $ slist [1..5]
Slist {sList = [0,1], sSize = Size 2}
>>> findIndices (<0) $ slist [1..5]
Slist {sList = [], sSize = Size 0}
>>> take 5 $ findIndices (<=10) $ infiniteSlist [1..]
Slist {sList = [0,1,2,3,4], sSize = Size 5}
-}
findIndices :: forall a . (a -> Bool) -> Slist a -> Slist Int
findIndices p (Slist l Infinity) = infiniteSlist $ L.findIndices p l
findIndices p Slist{..} = let (newS, newL) = go 0 0 sList in
    Slist newL (Size newS)
  where
    go :: Int -> Int -> [a] -> (Int, [Int])
    go !n _ [] = (n, [])
    go n !cur (x:xs) =
        if p x
        then second (cur:) $ go (n + 1) (cur + 1) xs
        else go n (cur + 1) xs
{-# INLINE findIndices #-}

----------------------------------------------------------------------------
-- Zipping
----------------------------------------------------------------------------

{- | @O(min n m)@.
Takes two slists and returns a slist of corresponding pairs.

>>> zip (slist [1,2]) (slist ["one", "two"])
Slist {sList = [(1,"one"),(2,"two")], sSize = Size 2}
>>> zip (slist [1,2,3]) (slist ["one", "two"])
Slist {sList = [(1,"one"),(2,"two")], sSize = Size 2}
>>> zip (slist [1,2]) (slist ["one", "two", "three"])
Slist {sList = [(1,"one"),(2,"two")], sSize = Size 2}
>>> zip mempty (slist [1..5])
Slist {sList = [], sSize = Size 0}
>>> zip (infiniteSlist [1..]) (slist ["one", "two"])
Slist {sList = [(1,"one"),(2,"two")], sSize = Size 2}
-}
zip :: Slist a -> Slist b -> Slist (a, b)
zip (Slist l1 s1) (Slist l2 s2) = Slist
    { sList = L.zip l1 l2
    , sSize = min s1 s2
    }
{-# INLINE zip #-}

{- | @O(minimum [n1, n2, n3])@.
Takes three slists and returns a slist of triples, analogous to 'zip'.
-}
zip3 :: Slist a -> Slist b -> Slist c -> Slist (a, b, c)
zip3 (Slist l1 s1) (Slist l2 s2) (Slist l3 s3) = Slist
    { sList = L.zip3 l1 l2 l3
    , sSize = minimum [s1, s2, s3]
    }
{-# INLINE zip3 #-}

{- | @O(min n m)@.
Generalises 'zip' by zipping with the given function, instead of a tupling function.

For example, @zipWith (+)@ is applied to two lists to produce the list of corresponding sums.
-}
zipWith :: (a -> b -> c) -> Slist a -> Slist b -> Slist c
zipWith f (Slist l1 s1) (Slist l2 s2) = Slist
    { sList = L.zipWith f l1 l2
    , sSize = min s1 s2
    }
{-# INLINE zipWith #-}

{- | @O(minimum [n1, n2, n3])@.
Takes a function which combines three elements, as well as three slists
and returns a slist of their point-wise combination, analogous to 'zipWith'.
-}
zipWith3 :: (a -> b -> c -> d) -> Slist a -> Slist b -> Slist c -> Slist d
zipWith3 f (Slist l1 s1) (Slist l2 s2) (Slist l3 s3) = Slist
    { sList = L.zipWith3 f l1 l2 l3
    , sSize = minimum [s1, s2, s3]
    }
{-# INLINE zipWith3 #-}

{- | @O(n)@.
Transforms a slist of pairs into a slist of first components
and a slist of second components.

>>> unzip $ slist [(1,"one"),(2,"two")]
(Slist {sList = [1,2], sSize = Size 2},Slist {sList = ["one","two"], sSize = Size 2})
-}
unzip :: Slist (a, b) -> (Slist a, Slist b)
unzip Slist{..} = let (as, bs) = L.unzip sList in (l as, l bs)
  where
    l :: [x] -> Slist x
    l x = Slist x sSize
{-# INLINE unzip #-}

{- | @O(n)@.
Takes a slist of triples and returns three slists, analogous to 'unzip'.
-}
unzip3 :: Slist (a, b, c) -> (Slist a, Slist b, Slist c)
unzip3 Slist{..} = let (as, bs, cs) = L.unzip3 sList in (l as, l bs, l cs)
  where
    l :: [x] -> Slist x
    l x = Slist x sSize
{-# INLINE unzip3 #-}

----------------------------------------------------------------------------
-- Sets
----------------------------------------------------------------------------

{- $sets

Set is a special case of slists so that it consist of the unique elements.

Example of set:

@
Slist {sList = "qwerty", sSize = Size 6}
Slist {sList = [1..], sSize = Infinity}
@
-}

{- | @O(n^2)@.
Removes duplicate elements from a slist. In particular,
it keeps only the first occurrence of each element.

It is a special case of 'nubBy', which allows to supply your own equality test.

>>> nub $ replicate 5 'a'
Slist {sList = "a", sSize = Size 1}
>>> nub mempty
Slist {sList = [], sSize = Size 0}
>>> nub $ slist [1,2,3,4,3,2,1,2,4,3,5]
Slist {sList = [1,2,3,4,5], sSize = Size 5}
-}
nub :: Eq a => Slist a -> Slist a
nub = nubBy (==)
{-# INLINE nub #-}

{- | @O(n^2)@.
Behaves just like 'nub', except it uses a user-supplied equality predicate
instead of the overloaded '==' function.

>>> nubBy (\x y -> mod x 3 == mod y 3) $ slist [1,2,4,5,6]
Slist {sList = [1,2,6], sSize = Size 3}
-}
nubBy :: forall a . (a -> a -> Bool) -> Slist a -> Slist a
nubBy f Slist{..} = let (s, l) = go 0 [] sList in case sSize of
    Infinity -> infiniteSlist l
    _        -> Slist l $ Size s
  where
    go :: Int -> [a] -> [a] -> (Int, [a])
    go !n res [] = (n, res)
    go n res (x:xs) =
        if any (f x) res
        then go n res xs
        else go (n + 1) (res ++ [x]) xs
{-# INLINE nubBy #-}

{- | Removes duplicate elements from a slist, keeping only the first occurance of
the element.

Like 'nub' but runs in \( O(n \log n) \)  time and requires 'Ord'.

>>> ordNub $ slist [3, 3, 3, 2, 2, -1, 1]
Slist {sList = [3,2,-1,1], sSize = Size 4}

-}
ordNub :: forall a . (Ord a) => Slist a -> Slist a
ordNub sl = let (s, l) = go 0 Set.empty (sList sl) in Slist
    { sList = l
    , sSize = Size s
    }
  where
    go :: Int -> Set.Set a -> [a] -> (Int, [a])
    go !i _ []     = (i, [])
    go i s (x:xs) =
      if x `Set.member` s
      then go i s xs
      else  second (x:) $ go (i + 1) (Set.insert x s) xs
{-# INLINEABLE ordNub #-}

{- | @O(n)@.
Removes the first occurrence of the given element from its slist argument.

>>> delete 'h' $ slist "hahaha"
Slist {sList = "ahaha", sSize = Size 5}
>>> delete 0 $ slist [1..3]
Slist {sList = [1,2,3], sSize = Size 3}
-}
delete :: Eq a => a -> Slist a -> Slist a
delete = deleteBy (==)
{-# INLINE delete #-}

{- | @O(n)@.
Behaves like 'delete', but takes a user-supplied equality predicate.

>>> deleteBy (>=) 4 $ slist [1..10]
Slist {sList = [2,3,4,5,6,7,8,9,10], sSize = Size 9}
-}
deleteBy :: forall a . (a -> a -> Bool) -> a -> Slist a -> Slist a
deleteBy f a (Slist l Infinity) = infiniteSlist $ L.deleteBy f a l
deleteBy f a Slist{..} = let (del, res) = go sList in
    Slist res $ sSize - del
  where
    go :: [a] -> (Size, [a])
    go [] = (Size 0, [])
    go (x:xs) = if f a x
        then (Size 1, xs)
        else second (x:) $ go xs
{-# INLINE deleteBy #-}

{- | @O(n*m)@.
Takes a predicate and two slists and returns the first slist
with the first occurrence of each element of the second slist removed.

>>> deleteFirstsBy (==) (slist [1..5]) (slist [2,8,4,10,1])
Slist {sList = [3,5], sSize = Size 2}
-}
deleteFirstsBy :: (a -> a -> Bool) -> Slist a -> Slist a -> Slist a
deleteFirstsBy f = foldr (deleteBy f)
{-# INLINE deleteFirstsBy #-}

{- | @O(n*m)@.
Returns the difference between two slists. The operation is non-associative.
In the result of @diff xs ys@, the first occurrence of each element of @ys@
in turn (if any) has been removed from @xs@. Thus

> diff (xs <> ys) ys == xs

>>> diff (slist [1..10]) (slist [1,3..10])
Slist {sList = [2,4,6,8,10], sSize = Size 5}
>>> diff (slist [1,3..10]) (slist [2,4..10])
Slist {sList = [1,3,5,7,9], sSize = Size 5}
-}
diff :: Eq a => Slist a -> Slist a -> Slist a
diff = foldr delete
{-# INLINE diff #-}

{- | @O(n*m)@.
Returns the list union of the two slists.

>>> union (slist "pen") (slist "apple")
Slist {sList = "penal", sSize = Size 5}

Duplicates, and elements of the first slist, are removed from the the second slist,
but if the first slist contains duplicates, so will the result.

>>> union (slist "apple") (slist "pen")
Slist {sList = "applen", sSize = Size 6}

It is a special case of 'unionBy'.
-}
union :: Eq a => Slist a -> Slist a -> Slist a
union = unionBy (==)
{-# INLINE union #-}

{- | @O(n*m)@.
Non-overloaded version of 'union'.
-}
unionBy :: (a -> a -> Bool) -> Slist a -> Slist a -> Slist a
unionBy f xs ys = xs <> deleteFirstsBy f (nubBy f ys) xs
{-# INLINE unionBy #-}

{- | @O(n*m)@.
Returns the slist intersection of two slists.

>>> intersect (slist [1,2,3,4]) (slist [2,4,6,8])
Slist {sList = [2,4], sSize = Size 2}

If the first list contains duplicates, so will the result.

>>> intersect (slist [1,2,2,3,4]) (slist [6,4,4,2])
Slist {sList = [2,2,4], sSize = Size 3}

If the first slist is infinite, so will be the result.

If the element is found in both the first and the second slist,
the element from the first slist will be used.

It is a special case of 'intersectBy'.
-}
intersect :: Eq a => Slist a -> Slist a -> Slist a
intersect = intersectBy (==)
{-# INLINE intersect #-}

{- | @O(n*m)@.
Non-overloaded version of 'intersect'.
-}
intersectBy :: forall a . (a -> a -> Bool) -> Slist a -> Slist a -> Slist a
intersectBy _ (Slist _ (Size 0)) _ = mempty
intersectBy _ _ (Slist _ (Size 0)) = mempty
intersectBy f (Slist l1 Infinity) (Slist l2 _) = infiniteSlist $ L.intersectBy f l1 l2
intersectBy f (Slist l1 _) (Slist l2 _) =
    let (s, l) = go 0 l1 in Slist l $ Size s
  where
    go :: Int -> [a] -> (Int, [a])
    go n [] = (n, [])
    go n (x:xs) =
        if any (f x) l2
        then second (x:) $ go (n + 1) xs
        else go n xs
{-# INLINE intersectBy #-}

----------------------------------------------------------------------------
-- Ordered slists
----------------------------------------------------------------------------

{- | @O(n log n)@.
implements a stable sorting algorithm. It is a special case of 'sortBy'.

Elements are arranged from from lowest to highest, keeping duplicates
in the order they appeared in the input.

>>> sort $ slist [10, 9..1]
Slist {sList = [1,2,3,4,5,6,7,8,9,10], sSize = Size 10}

/Note:/ this function hangs on infinite slists.
-}
sort :: Ord a => Slist a -> Slist a
sort = sortBy compare
{-# INLINE sort #-}

{- | @O(n log n)@.
Non-overloaded version of 'sort'.

>>> sortBy (\(a,_) (b,_) -> compare a b) $ slist [(2, "world"), (4, "!"), (1, "Hello")]
Slist {sList = [(1,"Hello"),(2,"world"),(4,"!")], sSize = Size 3}

/Note:/ this function hangs on infinite slists.
-}
sortBy :: (a -> a -> Ordering) -> Slist a -> Slist a
sortBy f Slist{..} = Slist (L.sortBy f sList) sSize
{-# INLINE sortBy #-}

{- | @O(n log n)@.
Sorts a list by comparing the results of a key function applied to each
element.  @sortOn f@ is equivalent to @'sortBy' (comparing f)@, but has the
performance advantage of only evaluating @f@ once for each element in the
input list.  This is called the decorate-sort-undecorate paradigm, or
Schwartzian transform.

Elements are arranged from lowest to highest, keeping duplicates in
the order they appeared in the input.

>>> sortOn fst $ slist [(2, "world"), (4, "!"), (1, "Hello")]
Slist {sList = [(1,"Hello"),(2,"world"),(4,"!")], sSize = Size 3}

/Note:/ this function hangs on infinite slists.
-}
sortOn :: Ord b => (a -> b) -> Slist a -> Slist a
sortOn f Slist{..} = Slist (L.sortOn f sList) sSize
{-# INLINE sortOn #-}

{- | @O(n log n)@.
Sorts a list by comparing the results of a key function applied to each
element.

Elements are arranged from lowest to highest, keeping duplicates in
the order they appeared in the input.

>>> sortWith fst $ slist [(2, "world"), (4, "!"), (1, "Hello")]
Slist {sList = [(1,"Hello"),(2,"world"),(4,"!")], sSize = Size 3}

@since x.x.x.x
-}
sortWith :: Ord b => (a -> b) -> Slist a -> Slist a
sortWith f Slist{..} = Slist (Exts.sortWith f sList) sSize
{-# INLINE sortWith #-}

{- | @O(n)@.
Takes an element and a slist and inserts the element into the slist
at the first position where it is less than or equal to the next element.
In particular, if the list is sorted before the call, the result will also
be sorted. It is a special case of 'insertBy'.

>>> insert 4 $ slist [1,2,3,5,6]
Slist {sList = [1,2,3,4,5,6], sSize = Size 6}
-}
insert :: Ord a => a -> Slist a -> Slist a
insert = insertBy compare
{-# INLINE insert #-}

-- | @O(n)@. The non-overloaded version of 'insert'.
insertBy :: (a -> a -> Ordering) -> a -> Slist a -> Slist a
insertBy f a Slist{..} = Slist (L.insertBy f a sList) (sSize + 1)
{-# INLINE insertBy #-}

----------------------------------------------------------------------------
-- Generic fuctions
----------------------------------------------------------------------------

{- | @O(1)@.
The 'genericLength' function is an overloaded version of 'length'.
In particular, instead of returning an 'Int', it returns any type which is an
instance of 'Num'.

>>> genericLength $ one 42
1
>>> genericLength $ slist [1..3]
3
>>> genericLength $ infiniteSlist [1..]
9223372036854775807
-}
genericLength :: Num i => Slist a -> i
genericLength = fromIntegral . length
{-# INLINE genericLength #-}

{- | @O(i) | i < n@ and @O(1) | otherwise@.
The 'genericTake' function is an overloaded version of 'take', which
accepts any 'Integral' value as the number of elements to take.

>>> genericTake 5 $ slist "Hello world!"
Slist {sList = "Hello", sSize = Size 5}
>>> genericTake 20 $ slist "small"
Slist {sList = "small", sSize = Size 5}
>>> genericTake 0 $ slist "none"
Slist {sList = "", sSize = Size 0}
>>> genericTake (-11) $ slist "hmm"
Slist {sList = "", sSize = Size 0}
>>> genericTake 4 $ infiniteSlist [1..]
Slist {sList = [1,2,3,4], sSize = Size 4}
-}
genericTake :: Integral i => i -> Slist a -> Slist a
genericTake (fromIntegral -> i) sl@Slist{..}
    | Size i >= sSize = sl
    | i <= 0 = mempty
    | otherwise = Slist
        { sList = L.genericTake i sList
        , sSize = min sSize (Size i)
        }
{-# INLINE genericTake #-}

{- | @O(i) | i < n@ and @O(1) | otherwise@.
The 'genericDrop' function is an overloaded version of 'drop', which accepts
any 'Integral' value as the number of elements to drop.

>>> genericDrop 6 $ slist "Hello World"
Slist {sList = "World", sSize = Size 5}
>>> genericDrop 42 $ slist "oops!"
Slist {sList = "", sSize = Size 0}
>>> genericDrop 0 $ slist "Hello World!"
Slist {sList = "Hello World!", sSize = Size 12}
>>> genericDrop (-4) $ one 42
Slist {sList = [42], sSize = Size 1}

@
>> __drop 5 $ 'infiniteSlist' [1..]__
Slist {sList = [6..], sSize = 'Infinity'}
@

-}
genericDrop :: Integral i => i -> Slist a -> Slist a
genericDrop (fromIntegral -> i) sl@Slist{..}
    | i <= 0 = sl
    | Size i >= sSize = mempty
    | otherwise = Slist
        { sList = L.genericDrop i sList
        , sSize = sSize - Size i
        }
{-# INLINE genericDrop #-}

{- | @O(i) | i < n@ and @O(1) | otherwise@.
The 'genericSplitAt' function is an overloaded version of 'splitAt', which
accepts any 'Integral' value as the position at which to split.

>>> genericSplitAt 5 $ slist "Hello World!"
(Slist {sList = "Hello", sSize = Size 5},Slist {sList = " World!", sSize = Size 7})
>>> genericSplitAt 0 $ slist "abc"
(Slist {sList = "", sSize = Size 0},Slist {sList = "abc", sSize = Size 3})
>>> genericSplitAt 4 $ slist "abc"
(Slist {sList = "abc", sSize = Size 3},Slist {sList = "", sSize = Size 0})
>>> genericSplitAt (-42) $ slist "??"
(Slist {sList = "", sSize = Size 0},Slist {sList = "??", sSize = Size 2})

@
>> __genericSplitAt 2 $ 'infiniteSlist' [1..]__
(Slist {sList = [1,2], sSize = 'Size' 2}, Slist {sList = [3..], sSize = 'Infinity'})
@

-}
genericSplitAt :: Integral i => i -> Slist a -> (Slist a, Slist a)
genericSplitAt (fromIntegral -> i) sl@Slist{..}
    | i <= 0 = (mempty, sl)
    | Size i >= sSize = (sl, mempty)
    | otherwise =
      let (l1, l2) = L.genericSplitAt i sList
          s2 = sSize - Size i
      in (Slist l1 $ Size i, Slist l2 s2)
{-# INLINE genericSplitAt #-}

{- | @O(i) | i < n@ and @O(1) | otherwise@.
The 'genericAt' function is an overloaded version of 'at', which
accepts any 'Integral' value as the position. If the element on the given
position does not exist it will return 'Nothing'.

>>> let sl = slist [1..10]
>>> genericAt 0 sl
Just 1
>>> genericAt (-1) sl
Nothing
>>> genericAt 11 sl
Nothing
>>> genericAt 9 sl
Just 10
-}
genericAt :: Integral i => i -> Slist a -> Maybe a
genericAt = at . fromIntegral
{-# INLINE genericAt #-}

{- | @O(min i n)@.
The 'genericUnsafeAt' function is an overloaded version of 'unsafeAt', which
accepts any 'Integral' value as the position. If the element on the given
position does not exist it throws the exception at run-time.

>>> let sl = slist [1..10]
>>> genericUnsafeAt 0 sl
1
>>> genericUnsafeAt (-1) sl
*** Exception: Slist.genericUnsafeAt: negative argument
>>> genericUnsafeAt 11 sl
*** Exception: Slist.genericUnsafeAt: index too large
>>> genericUnsafeAt 9 sl
10
-}
genericUnsafeAt :: Integral i => i -> Slist a -> a
genericUnsafeAt i _ | i < 0 = errorWithoutStackTrace "Slist.genericUnsafeAt: negative argument"
genericUnsafeAt i (Slist l Infinity) = L.genericIndex l i
genericUnsafeAt i (Slist l (Size n))
    | i >= fromIntegral n = errorWithoutStackTrace "Slist.genericUnsafeAt: index too large"
    | otherwise = L.genericIndex l i
{-# INLINE genericUnsafeAt #-}

{- | @O(n)@.
The 'genericReplicate' function is an overloaded version of 'replicate',
which accepts any 'Integral' value as the number of repetitions to make.

>>> genericReplicate 3 'o'
Slist {sList = "ooo", sSize = Size 3}
>>> genericReplicate (-11) "hmm"
Slist {sList = [], sSize = Size 0}
-}
genericReplicate :: Integral i => i -> a -> Slist a
genericReplicate n x
    | n <= 0 = mempty
    | otherwise = Slist (L.genericReplicate n x) $ Size (fromIntegral n)
{-# INLINE genericReplicate #-}
