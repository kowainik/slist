# Changelog

`slist` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## Unreleased

* [#45](https://github.com/kowainik/slist/issues/45):
  Support GHC-9.0. Update older GHC's bounds.
* [#30](https://github.com/kowainik/slist/issues/30):
  Add the `cons` function.
* [#34](https://github.com/kowainik/slist/issues/34):
  Add `partitionWith` and `listPartitionWith`.
* [#29](https://github.com/kowainik/slist/issues/29):
  Add `Slist.Maybe` module with `maybeToSlist`, `slistToMaybe`, `catMaybes`,
  `mapMaybe`, `slistWith` functions.
* [#31](https://github.com/kowainik/slist/issues/31):
  Add `sortWith`.
* [#24](https://github.com/kowainik/slist/issues/24):
  Add `chunksOf` for `Slist` and `listChunksOf` for ordinary lists.
* Move the `Slist` data type into the separate `Slist.Type` module.

## 0.1.1.0 — Apr 18, 2020

* Fix `mconcat` for `Slist` `Monoid` instance.
* [#25](https://github.com/kowainik/slist/issues/25):
  Support GHC-8.10.
* Update to GHC-8.8.3 from GHC-8.8.1.

## 0.1.0.0

* [#13](https://github.com/kowainik/slist/issues/13):
  Support GHC-8.8.1.
* [#16](https://github.com/kowainik/slist/issues/16):
  Use `DerivingStrategies`.
* [#9](https://github.com/kowainik/slist/issues/9):
  Implement `fromRange` function.
  (by @zfnmxt)
* [#6](https://github.com/kowainik/slist/issues/6):
  Add generic function over the size and indices.
  (by @waynee95)
* Make `dropWhile` work better on infinite lists.
  (by @chshersh)
* Support GHC-8.6.5 instead of GHC-8.6.3.
* [#6](https://github.com/kowainik/slist/issues/6):
  Build with Stack.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/slist/releases
