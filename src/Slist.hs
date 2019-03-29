{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Slist
       ( -- * Types
         Size
       , Slist
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
         -- * Basic functions
       , len
       , size
       , isNull
       , head
       , safeHead
       , last
       , safeLast
       , init
       , tail
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
       , concatMap

         -- * Building slists
         -- ** Scans
         -- ** Accumulating maps
         -- ** Unfolding

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

         -- * Sets: special slists
       , nub
       , nubBy
       , delete
       , deleteBy
       , deleteFirstsBy
       , diff
       , union
       , unionBy
       , intersect
       , intersectBy
       ) where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Data.Bifunctor (bimap, first, second)
#if ( __GLASGOW_HASKELL__ == 802 )
import Data.Semigroup (Semigroup (..))
#endif
import Prelude hiding (break, concat, concatMap, cycle, drop, dropWhile, filter, head, init,
                iterate, last, lookup, map, repeat, replicate, reverse, span, splitAt, tail, take,
                takeWhile, unzip, unzip3, zip, zip3, zipWith, zipWith3)

import Slist.Size (Size (..), sizeMin, sizes)

import qualified Data.Foldable as F (Foldable (..))
import qualified Data.List as L
import qualified GHC.Exts as L (IsList (..))
import qualified Prelude as P


data Slist a = Slist
    { sList :: [a]
    , sSize :: Size
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
    null = isNull
    {-# INLINE null #-}

    length :: Slist a -> Int
    length = len
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

iterate :: (a -> a) -> a -> Slist a
iterate f = infiniteSlist . L.iterate f
{-# INLINE iterate #-}

#if ( __GLASGOW_HASKELL__ > 802 )
iterate' :: (a -> a) -> a -> Slist a
iterate' f = infiniteSlist . L.iterate' f
{-# INLINE iterate' #-}
#endif

repeat :: a -> Slist a
repeat = infiniteSlist . L.repeat
{-# INLINE repeat #-}

replicate :: Int -> a -> Slist a
replicate n x = Slist (L.replicate n x) $ Size n
{-# INLINE replicate #-}

cycle :: Slist a -> Slist a
cycle Slist{..} = infiniteSlist $ L.cycle sList
{-# INLINE cycle #-}

----------------------------------------------------------------------------
-- Basic functions
----------------------------------------------------------------------------

{- | Returns the length of a structure as an 'Int'.
Runs in @O(1)@ time. On infinite lists returns the 'Int's 'maxBound'.

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

size :: Slist a -> Size
size = sSize
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
    _      -> Slist (P.drop 1 sList) (sSize - 1)
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

----------------------------------------------------------------------------
-- Transformations
----------------------------------------------------------------------------

map :: (a -> b) -> Slist a -> Slist b
map f Slist{..} = Slist (P.map f sList) sSize
{-# INLINE map #-}

reverse :: Slist a -> Slist a
reverse Slist{..} = Slist (L.reverse sList) sSize
{-# INLINE reverse #-}

safeReverse :: Slist a -> Slist a
safeReverse sl@(Slist _ Infinity) = sl
safeReverse sl                    = reverse sl
{-# INLINE safeReverse #-}

intersperse :: a -> Slist a -> Slist a
intersperse _ sl@(Slist _ (Size 0)) = sl
intersperse a Slist{..}             = Slist (L.intersperse a sList) (2 * sSize - 1)
{-# INLINE intersperse #-}

intercalate :: Slist a -> Slist (Slist a) -> Slist a
intercalate x = foldr (<>) mempty . intersperse x
{-# INLINE intercalate #-}

{- | The transpose function transposes the rows and columns of its argument. For example,

>>> transpose [[1,2,3],[4,5,6]]
[[1,4],[2,5],[3,6]]
If some of the rows are shorter than the following rows, their elements are skipped:

>>> transpose [[10,11],[20],[],[30,31,32]]
[[10,20,30],[11,31],[32]]
-}
transpose :: Slist (Slist a) -> Slist (Slist a)
transpose (Slist l _) = Slist
    { sList = P.map slist $ L.transpose $ P.map sList l
    , sSize = maximum $ P.map sSize l
    }
{-# INLINE transpose #-}

subsequences :: Slist a -> Slist (Slist a)
subsequences Slist{..} = Slist
    { sList = P.map slist $ L.subsequences sList
    , sSize = newSize sSize
    }
  where
    newSize :: Size -> Size
    newSize Infinity = Infinity
    newSize (Size n) = Size $ 2 ^ (toInteger n)
{-# INLINE subsequences #-}

permutations :: Slist a -> Slist (Slist a)
permutations (Slist l s) = Slist
    { sList = P.map (\a -> Slist a s) $ L.permutations l
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

concat :: Foldable t => t (Slist a) -> Slist a
concat = foldr (<>) mempty
{-# INLINE concat #-}

concatMap :: Foldable t => (a -> Slist b) -> t a -> Slist b
concatMap f = foldMap f
{-# INLINE concatMap #-}

----------------------------------------------------------------------------
-- Building lists
----------------------------------------------------------------------------

-- TODO: Scans
-- TODO: accumulating slists

----------------------------------------------------------------------------
-- Sublists
----------------------------------------------------------------------------

take :: Int -> Slist a -> Slist a
take i sl@Slist{..} =
    if Size i >= sSize
    then sl
    else Slist
        { sList = P.take i sList
        , sSize = sizeMin i sSize
        }
{-# INLINE take #-}

drop :: Int -> Slist a -> Slist a
drop i sl@Slist{..}
    | i <= 0 = sl
    | Size i >= sSize = mempty
    | otherwise = Slist
        { sList = P.drop i sList
        , sSize = max (Size 0) $ sSize - Size i
        }
{-# INLINE drop #-}

splitAt :: Int -> Slist a -> (Slist a, Slist a)
splitAt i sl@Slist{..}
    | i <= 0 = (mempty, sl)
    | Size i >= sSize = (sl, mempty)
    | otherwise =
        let (l1, l2) = P.splitAt i sList
            s2 = sSize - Size i
        in (Slist l1 $ Size i, Slist l2 s2)
{-# INLINE splitAt #-}

takeWhile :: forall a . (a -> Bool) -> Slist a -> Slist a
takeWhile p Slist{..} = let (s, l) = go 0 sList in Slist l $ Size s
  where
    go :: Int -> [a] -> (Int, [a])
    go !n [] = (n, [])
    go !n (x:xs) =
        if p x
        then let (i, l) = go (n + 1) xs in (i, x:l)
        else (n, [])
{-# INLINE takeWhile #-}

dropWhile :: forall a . (a -> Bool) -> Slist a -> Slist a
dropWhile p Slist{..} = let (s, l) = go 0 sList in Slist l $ max (Size 0) (sSize - Size s)
  where
    go :: Int -> [a] -> (Int, [a])
    go !n [] = (n, [])
    go !n (x:xs) =
        if p x
        then go (n + 1) xs
        else (n, x:xs)
{-# INLINE dropWhile #-}

span :: forall a . (a -> Bool) -> Slist a -> (Slist a, Slist a)
span p Slist{..} = let (s, l, r) = go 0 sList in
    ( Slist l $ Size s
    , Slist r $ max (Size 0) (sSize - Size s)
    )
  where
    go :: Int -> [a] -> (Int, [a], [a])
    go !n [] = (n, [], [])
    go !n (x:xs) =
        if p x
        then let (s, l, r) = go (n + 1) xs in (s, x:l, r)
        else (n, [], x:xs)
{-# INLINE span #-}

break :: (a -> Bool) -> Slist a -> (Slist a, Slist a)
break p = span (not . p)
{-# INLINE break #-}

stripPrefix :: Eq a => Slist a -> Slist a -> Maybe (Slist a)
stripPrefix (Slist l1 s1) f@(Slist l2 s2)
    | s1 == Size 0 = Just f
    | s1 > s2 = Nothing
    | otherwise = (\l -> Slist l $ s2 - s1) <$> L.stripPrefix l1 l2
{-# INLINE stripPrefix #-}

safeStripPrefix :: Eq a => Slist a -> Slist a -> Maybe (Slist a)
safeStripPrefix (Slist _ Infinity) (Slist _ Infinity) = Nothing
safeStripPrefix sl1 sl2                               = stripPrefix sl1 sl2
{-# INLINE safeStripPrefix #-}

group :: Eq a => Slist a -> Slist (Slist a)
group = groupBy (==)
{-# INLINE group #-}

groupBy :: (a -> a -> Bool) -> Slist a -> Slist (Slist a)
groupBy p (Slist l Infinity) = infiniteSlist $ P.map slist $ L.groupBy p l
groupBy p Slist{..}          = slist $ P.map slist $ L.groupBy p sList
{-# INLINE groupBy #-}

inits :: Slist a -> Slist (Slist a)
inits (Slist l s) = Slist
    { sList = L.zipWith Slist (L.inits l) $ sizes s
    , sSize = s + 1
    }
{-# INLINE inits #-}

tails :: Slist a -> Slist (Slist a)
tails (Slist l Infinity) = infiniteSlist $ P.map infiniteSlist (L.tails l)
tails (Slist l s@(Size n)) = Slist
    { sList = L.zipWith (\li i -> Slist li $ Size i) (L.tails l) [n, n - 1 .. 0]
    , sSize = s + 1
    }
{-# INLINE tails #-}


isPrefixOf :: Eq a => Slist a -> Slist a -> Bool
isPrefixOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = L.isPrefixOf l1 l2
{-# INLINE isPrefixOf #-}

safeIsPrefixOf :: Eq a => Slist a -> Slist a -> Bool
safeIsPrefixOf sl1@(Slist _ s1) sl2@(Slist _ s2)
    | s1 == Infinity && s2 == Infinity = False
    | otherwise = isPrefixOf sl1 sl2
{-# INLINE safeIsPrefixOf #-}

isSuffixOf :: Eq a => Slist a -> Slist a -> Bool
isSuffixOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = L.isSuffixOf l1 l2
{-# INLINE isSuffixOf #-}

safeIsSuffixOf :: Eq a => Slist a -> Slist a -> Bool
safeIsSuffixOf sl1 sl2@(Slist _ s2)
    | s2 == Infinity = False
    | otherwise = isSuffixOf sl1 sl2
{-# INLINE safeIsSuffixOf #-}

isInfixOf :: Eq a => Slist a -> Slist a -> Bool
isInfixOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = L.isInfixOf l1 l2
{-# INLINE isInfixOf #-}

safeIsInfixOf :: Eq a => Slist a -> Slist a -> Bool
safeIsInfixOf sl1@(Slist _ s1) sl2@(Slist _ s2)
    | s1 == Infinity && s2 == Infinity = False
    | otherwise = isInfixOf sl1 sl2
{-# INLINE safeIsInfixOf #-}

isSubsequenceOf :: Eq a => Slist a -> Slist a -> Bool
isSubsequenceOf (Slist l1 s1) (Slist l2 s2)
    | s1 > s2 = False
    | otherwise = L.isSubsequenceOf l1 l2
{-# INLINE isSubsequenceOf #-}

safeIsSubsequenceOf :: Eq a => Slist a -> Slist a -> Bool
safeIsSubsequenceOf sl1@(Slist _ s1) sl2@(Slist _ s2)
    | s1 == Infinity && s2 == Infinity = False
    | otherwise = isSubsequenceOf sl1 sl2
{-# INLINE safeIsSubsequenceOf #-}

----------------------------------------------------------------------------
-- Searching
----------------------------------------------------------------------------

lookup :: Eq a => a -> Slist (a, b) -> Maybe b
lookup a = L.lookup a . sList
{-# INLINE lookup #-}

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

----------------------------------------------------------------------------
-- Indexing
----------------------------------------------------------------------------

at :: Int -> Slist a -> Maybe a
at n Slist{..}
    | n < 0 || Size n >= sSize = Nothing
    | otherwise = Just $ sList L.!! n
{-# INLINE at #-}

unsafeAt :: Int -> Slist a -> a
unsafeAt n Slist{..} = sList L.!! n
{-# INLINE unsafeAt #-}

elemIndex :: Eq a => a -> Slist a -> Maybe Int
elemIndex a = L.elemIndex a . sList
{-# INLINE elemIndex #-}

elemIndices :: Eq a => a -> Slist a -> Slist Int
elemIndices a = findIndices (a ==)
{-# INLINE elemIndices #-}

findIndex :: (a -> Bool) -> Slist a -> Maybe Int
findIndex p = L.findIndex p . sList
{-# INLINE findIndex #-}

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

zip :: Slist a -> Slist b -> Slist (a, b)
zip (Slist l1 s1) (Slist l2 s2) = Slist
    { sList = L.zip l1 l2
    , sSize = min s1 s2
    }
{-# INLINE zip #-}

zip3 :: Slist a -> Slist b -> Slist c -> Slist (a, b, c)
zip3 (Slist l1 s1) (Slist l2 s2) (Slist l3 s3) = Slist
    { sList = L.zip3 l1 l2 l3
    , sSize = minimum [s1, s2, s3]
    }
{-# INLINE zip3 #-}

zipWith :: (a -> b -> c) -> Slist a -> Slist b -> Slist c
zipWith f (Slist l1 s1) (Slist l2 s2) = Slist
    { sList = L.zipWith f l1 l2
    , sSize = min s1 s2
    }
{-# INLINE zipWith #-}

zipWith3 :: (a -> b -> c -> d) -> Slist a -> Slist b -> Slist c -> Slist d
zipWith3 f (Slist l1 s1) (Slist l2 s2) (Slist l3 s3) = Slist
    { sList = L.zipWith3 f l1 l2 l3
    , sSize = minimum [s1, s2, s3]
    }
{-# INLINE zipWith3 #-}

unzip :: Slist (a, b) -> (Slist a, Slist b)
unzip Slist{..} = let (as, bs) = L.unzip sList in (l as, l bs)
  where
    l :: [x] -> Slist x
    l x = Slist x sSize
{-# INLINE unzip #-}

unzip3 :: Slist (a, b, c) -> (Slist a, Slist b, Slist c)
unzip3 Slist{..} = let (as, bs, cs) = L.unzip3 sList in (l as, l bs, l cs)
  where
    l :: [x] -> Slist x
    l x = Slist x sSize
{-# INLINE unzip3 #-}

----------------------------------------------------------------------------
-- Sets
----------------------------------------------------------------------------

nub :: Eq a => Slist a -> Slist a
nub = nubBy (==)
{-# INLINE nub #-}

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

delete :: Eq a => a -> Slist a -> Slist a
delete = deleteBy (==)
{-# INLINE delete #-}

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

deleteFirstsBy :: (a -> a -> Bool) -> Slist a -> Slist a -> Slist a
deleteFirstsBy f = foldr (deleteBy f)
{-# INLINE deleteFirstsBy #-}

diff :: Eq a => Slist a -> Slist a -> Slist a
diff = foldr delete
{-# INLINE diff #-}

union :: Eq a => Slist a -> Slist a -> Slist a
union = unionBy (==)
{-# INLINE union #-}

unionBy :: (a -> a -> Bool) -> Slist a -> Slist a -> Slist a
unionBy f xs ys = xs <> deleteFirstsBy f (nubBy f ys) xs
{-# INLINE unionBy #-}

intersect :: Eq a => Slist a -> Slist a -> Slist a
intersect = intersectBy (==)
{-# INLINE intersect #-}

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
