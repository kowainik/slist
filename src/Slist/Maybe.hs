{- |
Copyright:  (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Useful 'Maybe' combinators to work with the 'Maybe' data type and 'Slist'
together.
-}
module Slist.Maybe
    ( maybeToSlist
    , slistToMaybe
    , catMaybes
    , mapMaybe
    , slistWith
    ) where

import Slist.Type (Slist (..), cons, one)


{- | Returns an empty list when given 'Nothing' or a singleton list when given
'Just'.

>>> maybeToSlist (Just 42)
Slist {sList = [42], sSize = Size 1}
>>> maybeToSlist Nothing
Slist {sList = [], sSize = Size 0}
-}
maybeToSlist :: Maybe a -> Slist a
maybeToSlist Nothing  = mempty
maybeToSlist (Just x) = one x
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
maybeToList . slistToMaybe {empty, singleton slist} = id
@
-}
slistToMaybe :: Slist a -> Maybe a
slistToMaybe = foldr (const . Just) Nothing
{-# INLINE slistToMaybe #-}
-- We define listToMaybe using foldr so that it can fuse via the foldr/build
-- rule. See #14387


{- | Takes a slist of 'Maybe's and returns a slist of all the 'Just' values.

>>> catMaybes (cons (Just 1) $ cons Nothing $ one $ Just 3)
Slist {sList = [1,3], sSize = Size 2}
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
-}
mapMaybe :: forall b a . (a -> Maybe b) -> Slist a -> Slist b
mapMaybe _ (Slist [] _) = mempty
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
-}
slistWith :: forall b a . (a -> Maybe b) -> [a] -> Slist b
slistWith _ [] = mempty
slistWith f (x:xs) = case f x of
    Nothing -> rest
    Just r  -> cons r rest
  where
    rest :: Slist b
    rest = slistWith f xs

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
