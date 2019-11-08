-- | Lists size representation

module Slist.Size
       ( Size (..)
       , sizes
       ) where


{- | Data type that represents lists size/lengths.

+-----------+----------+------------+
| List      | @length@ | Size       |
+===========+==========+============+
| @[]@      | @0@      | @Size 0@   |
+-----------+----------+------------+
| @[1..10]@ | @10@     | @Size 10@  |
+-----------+----------+------------+
| @[1..]@   | /hangs/  | @Infinity@ |
+-----------+----------+------------+

Note, that size is not suppose to have negative value, so use
the 'Size' constructor carefully.
-}
data Size
    -- | Finite size
    = Size !Int
    -- | Infinite size.
    | Infinity
    deriving stock (Show, Read, Eq, Ord)

{- | Efficient implementations of numeric operations with 'Size's.

Any operations with 'Infinity' size results into 'Infinity'.

TODO: checking on overflow when '+' or '*' sizes.
-}
instance Num Size where
    (+) :: Size -> Size -> Size
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    (Size x) + (Size y) = Size $ x + y
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

{- | The minimum possible size for the list is empty list: @Size 0@
The maximum possible size is 'Infinity'.
-}
instance Bounded Size where
    minBound :: Size
    minBound = Size 0

    maxBound :: Size
    maxBound = Infinity

{- | Returns the list of sizes from zero to the given one (including).

>>> sizes $ Size 3
[Size 0,Size 1,Size 2,Size 3]

@
>> __sizes Infinity__
[Size 0, Size 1, ..., Size 'maxBound', Infinity]
@
-}
sizes :: Size -> [Size]
sizes (Size n) = map Size [0..n]
sizes Infinity = map Size [0..maxBound] ++ [Infinity]
