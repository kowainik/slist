module Main (main) where

import Slist as  SL
import           Hedgehog
import           Hedgehog.Main (defaultMain)
import           Hedgehog.Classes
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = defaultMain $
  lawsCheck <$>
    [ functorLaws genFiniteSlist
    , applicativeLaws genFiniteSlist
    , monadLaws genFiniteSlist
    , foldableLaws genFiniteSlist
    , traversableLaws genFiniteSlist
    , alternativeLaws genFiniteSlist
    , semigroupLaws (genFiniteSlist genInt)
    , monoidLaws (genFiniteSlist genInt)
    ]

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 100)

genFiniteSlist :: Gen a -> Gen (Slist a)
genFiniteSlist gen = slist <$> Gen.list (Range.linear 0 100) gen
