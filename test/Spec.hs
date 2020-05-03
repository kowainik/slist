module Main (main) where

import Test.Hspec (hspec)

import Test.Slist.Size (sizeSpec)


main :: IO ()
main = hspec sizeSpec
