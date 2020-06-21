# slist

[![GitHub CI](https://github.com/kowainik/slist/workflows/CI/badge.svg)](https://github.com/kowainik/slist/actions)
[![Build status](https://img.shields.io/travis/kowainik/slist.svg?logo=travis)](https://travis-ci.org/kowainik/slist)
[![Hackage](https://img.shields.io/hackage/v/slist.svg?logo=haskell)](https://hackage.haskell.org/package/slist)
[![Stackage LTS](http://stackage.org/package/slist/badge/lts)](http://stackage.org/lts/package/slist)
[![Stackage Nightly](http://stackage.org/package/slist/badge/nightly)](http://stackage.org/nightly/package/slist)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

> ⚠️ Caution: this is a very opinionated library. There is no intention to replace the standard list data type.
> We are aware of every design decision we made for this package, and we are taking responsibility for that design.
> If you find it inappropriate, please, consider to use another library instead, that would fulfil your requirements.

This package introduces sized list data type — `Slist`. The data type
has the following shape:

```haskell
data Slist a = Slist
    { sList :: [a]
    , sSize :: Size
    }
```

As you can see that along with the familiar list, it contains `Size` field that
represents the size of the structure. Slists can be finite or infinite, and this
is expressed with `Size`.

```haskell
data Size
    = Size Int
    | Infinity
```

> ⚠️ Caution: `Int` is used for the size by design. We had to make some trade-offs
> to provide the better (as we think) interface for users. For more details on the
> choice, see the Haddock documentation for the `Size` data type.

This representation of the list gives some additional advantages. Getting the
length of the list is the "free" operation (runs in `O(1)`). This property
helps to improve the performance for a bunch of functions like `take`, `drop`,
`at`, etc. But also it doesn't actually add any overhead on the existing
functions.

Also, this allows to write a number of safe functions like `safeReverse`,
`safeHead`, `safeLast`, `safeIsSuffixOf`, etc.

## Comparison

Check out the comparison table between lists and slists performance.

| Function          | list (finite)                     | list (infinite)             | Slist (finite)                         | Slist (infinite) |
|-------------------|-----------------------------------|-----------------------------|----------------------------------------|------------------|
| `length`          | `O(n)`                            | <_hangs_>                   | `O(1)`                                 | `O(1)`           |
| `safeLast`        | `O(n)`                            | <_hangs_>                   | `O(n)`                                 | `O(1)`           |
| `init`            | `O(n)`                            | <_works infinitely_>        | `O(n)`                                 | `O(1)`           |
| `take`            | `O(min i n)`                      | `O(i)`                      | `0 < i < n`: `O(i)`; otherwise: `O(1)` | `O(i)`           |
| `at`              | `O(min i n)` (run-time exception) | `O(i)` (run-time exception) | `0 < i < n`: `O(i)`; otherwise: `O(1)` | `O(i)`           |
| `safeStripPrefix` | `O(m)`                            | `O(m)` (can hang)           | `O(m)`                                 | `O(m)`           |

## Potential usage cases

* When you ask the size of the list too frequently.
* When you need to convert to data structures that require to know the list
  size in advance for allocating an array of the elements.
  _Example:_ [Vector data structure](https://hackage.haskell.org/package/vector).
* When you need to serialise lists.
* When you need to control the behaviour depending on the finiteness of the list.
* When you need a more efficient or safe implementation of some functions.
