{-# LANGUAGE BangPatterns #-}

{- |
Copyright:  (c) 2021-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Useful 'Maybe' combinators to work with the 'Maybe' data type and 'Slist'
together.

@since 0.2.0.0
-}
module Slist.Maybe
    ( maybeToSlist
    , slistToMaybe
    , catMaybes
    , mapMaybe
    , slistWith
    ) where

import Data.Bifunctor (second)

import Slist.Size (Size (..))
import Slist.Type (Slist (..), cons, one)

import qualified Data.Maybe as M


{- | Returns an empty list when given 'Nothing' or a singleton list when given
'Just'.

>>> maybeToSlist (Just 42)
Slist {sList = [42], sSize = Size 1}
>>> maybeToSlist Nothing
Slist {sList = [], sSize = Size 0}

@since 0.2.0.0
-}
maybeToSlist :: Maybe a -> Slist a
maybeToSlist = maybe mempty one
{-# INLINE maybeToSlist #-}

{- | Returns 'Nothing' on an empty list or @'Just' a@ where @a@ is the first
element of the slist.

==== __Examples__

Basic usage:

>>> slistToMaybe mempty
Nothing

>>> slistToMaybe (one 42)
Just 42

>>> slistToMaybe (cons 1 $ cons 2 $ one 3)
Just 1

__Laws__ :

@
slistToMaybe . maybeToList ≡ id
@

Reverse is right only on singleton/empty lists

@
maybeToList . slistToMaybe {empty, singleton slist} ≡ {empty, singleton slist}
@

@since 0.2.0.0
-}
slistToMaybe :: Slist a -> Maybe a
slistToMaybe = foldr (const . Just) Nothing
{-# INLINE slistToMaybe #-}
-- We define listToMaybe using foldr so that it can fuse via the foldr/build
-- rule. See #14387


{- | Takes a slist of 'Maybe's and returns a slist of all the 'Just' values.

>>> catMaybes (cons (Just 1) $ cons Nothing $ one $ Just 3)
Slist {sList = [1,3], sSize = Size 2}

@since 0.2.0.0
-}
catMaybes :: Slist (Maybe a) -> Slist a
catMaybes = mapMaybe id
{-# INLINE catMaybes #-}

{- | The 'Maybe' version of 'map' which can throw out elements.

If appliying the given function returns 'Nothing', no element is added on to the
result list. If it is @'Just' b@, then @b@ is included in the result list.

>>> maybeEven x = if even x then Just x else Nothing
>>> s = cons 1 $ cons 2 $ one 3

>>> mapMaybe maybeEven s
Slist {sList = [2], sSize = Size 1}

If we map the 'Just' constructor, the entire list should be returned:

>>> mapMaybe Just s
Slist {sList = [1,2,3], sSize = Size 3}

@since 0.2.0.0
-}
mapMaybe :: forall b a . (a -> Maybe b) -> Slist a -> Slist b
mapMaybe _ (Slist [] _) = mempty
mapMaybe f (Slist xs Infinity) = Slist (M.mapMaybe f xs) Infinity
mapMaybe f (Slist (x:xs) n) = case f x of
    Nothing -> rest
    Just r  -> cons r rest
  where
    rest :: Slist b
    rest = mapMaybe f (Slist xs (n - 1))
{-# NOINLINE [1] mapMaybe #-}

{- | Similar to 'mapMaybe' but works with the ordinary list as the input:

>>> maybeEven x = if even x then Just x else Nothing

>>> slistWith maybeEven [1,2,3]
Slist {sList = [2], sSize = Size 1}

@since 0.2.0.0
-}
slistWith :: forall b a . (a -> Maybe b) -> [a] -> Slist b
slistWith f l = let (n, sl) = go 0 l in Slist sl (Size n)
  where
    go :: Int -> [a] -> (Int, [b])
    go !accSize [] = (accSize, [])
    go accSize (x:xs) = case f x of
        Nothing -> go accSize xs
        Just r  -> second (r:) $ go (accSize + 1) xs

-- {-# RULES
-- "mapMaybe"     [~1] forall f xs. mapMaybe f xs
--                     = build (\c n -> foldr (mapMaybeFB c f) n xs)
-- "mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
--   #-}
-- {-# INLINE [0] mapMaybeFB #-} -- See Note [Inline FB functions] in GHC.List
-- mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
-- mapMaybeFB cons f x next = case f x of
--   Nothing -> next
--   Just r -> cons r next
