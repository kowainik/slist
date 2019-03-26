module Slist.Size
       ( Size (..)
       , sizeMin
       ) where


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

-- | Returns the minimum size.
sizeMin :: Int -> Size -> Size
sizeMin i s = Size $ max 0 $ case s of
    Infinity -> i
    Size n   -> min i n
