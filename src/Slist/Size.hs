module Slist.Size
       ( Size (..)
       , sizeMin
       , sizes
       ) where


data Size
    = Size !Int
    | Infinity
    deriving (Show, Read, Eq, Ord)

instance Num Size where
    (+) :: Size -> Size -> Size
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    (Size x) + (Size y) = let res = x + y in
        -- checking on the overflowing
        if res < 0
        then Infinity
        else Size res
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

instance Bounded Size where
    minBound :: Size
    minBound = Size 0

    maxBound :: Size
    maxBound = Infinity

-- | Returns the minimum size.
sizeMin :: Int -> Size -> Size
sizeMin i s = Size $ max 0 $ case s of
    Infinity -> i
    Size n   -> min i n

-- | Returns the list of sizes from zero to given one (including).
sizes :: Size -> [Size]
sizes (Size n) = map Size [0..n]
sizes Infinity = map Size [0..maxBound] ++ [Infinity]
