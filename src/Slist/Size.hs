{- |
Copyright:  (c) 2019-2020 Veronika Romashkina
            (c) 2020-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lists size representation.
-}

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

Any operations with 'Infinity' size results into 'Infinity'. When
'Infinity' is a left argument, all operations are also
right-lazy. Operations are checked for integral overflow under the
assumption that all values inside 'Size' are positive.

>>> Size 10 + Size 5
Size 15
>>> Size 5 * Infinity
Infinity
>>> Infinity + error "Unevaluated size"
Infinity
>>> Size (10 ^ 10) * Size (10 ^ 10)
Infinity
-}
instance Num Size where
    (+) :: Size -> Size -> Size
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    (Size x) + (Size y) =
        if x + y < x  -- integer overflow
        then Infinity
        else Size $ x + y
    {-# INLINE (+) #-}

    (-) :: Size -> Size -> Size
    Infinity - _        = Infinity
    _ - Infinity        = Infinity
    (Size x) - (Size y) = Size (x - y)
    {-# INLINE (-) #-}

    (*) :: Size -> Size -> Size
    Infinity * _ = Infinity
    _ * Infinity = Infinity
    (Size x) * (Size y)
        | x == 0 || y == 0 = 0
        | otherwise =
            let result = x * y in
            if x == result `div` y
            then Size (x * y)
            else Infinity  -- multiplication overflow
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
