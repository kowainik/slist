# slist

[![Hackage](https://img.shields.io/hackage/v/slist.svg)](https://hackage.haskell.org/package/slist)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/vrom911/slist.svg)](https://travis-ci.org/vrom911/slist)

This package introduces sized list data type — `Slist`. The data type
has the following shape:

```haskell
data Slist a = Slist
    { sList :: [a]
    , sSize :: Size
    }
```

As you can see along with the familiar list, it contains `Size` field that
represents the size of the structure. Slists can be finite or infinite, and this
is expressed with `Size`.

```haskell
data Size
    = Size Int
    | Infinity
```

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

* When you ask the length of the list too frequently.
* When you need to convert to data structures that require to know the list
  size in advance for allocating an array of the elements.
  _Example:_ [Vector data structure](https://hackage.haskell.org/package/vector).
* When you need to serialised lists.
* When you need to control the behaviour depending on the finiteness of the list.
* When you need a more efficient or safe implementation of some functions.
