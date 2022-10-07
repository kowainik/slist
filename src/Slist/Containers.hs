{- |
Copyright:  (c) 2021-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Useful combinators to work with the data structures from @containers@ package
and 'Slist' together.

@since 0.2.0.0
-}
module Slist.Containers
    ( -- * Map
      mapToVals
    , mapToKeys
    , mapToPairs

      -- * Set
    , setToSlist
    ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Slist.Size (Size (..))
import Slist.Type (Slist (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


{- | @O(n)@.
Returns a 'Slist' of all values of the map in the ascending order of their keys.

@since 0.2.0.0
-}
mapToVals :: Map k v -> Slist v
mapToVals m = Slist
    { sList = Map.elems m
    , sSize = Size $ Map.size m
    }
{-# INLINE mapToVals #-}

{- | @O(n)@.
Returns a 'Slist' of all keys of the map in the ascending order.

@since 0.2.0.0
-}
mapToKeys :: Map k v -> Slist k
mapToKeys m = Slist
    { sList = Map.keys m
    , sSize = Size $ Map.size m
    }
{-# INLINE mapToKeys #-}

{- | @O(n)@.
Returns a 'Slist' of all key-value pairs of the map in the ascending order of their keys.

@since 0.2.0.0
-}
mapToPairs :: Map k v -> Slist (k, v)
mapToPairs m = Slist
    { sList = Map.toAscList m
    , sSize = Size $ Map.size m
    }
{-# INLINE mapToPairs #-}

{- | @O(n)@.
Returns a 'Slist' of all elements of the set in the ascending order.

@since 0.2.0.0
-}
setToSlist :: Set a -> Slist a
setToSlist s = Slist
    { sList = Set.elems s
    , sSize = Size $ Set.size s
    }
{-# INLINE setToSlist #-}
