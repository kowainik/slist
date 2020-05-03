{- HLINT ignore "Redundant fromInteger" -}
{- HLINT ignore "Reduce duplication" -}

module Test.Slist.Size
    ( sizeSpec
    ) where

import Hedgehog (Gen, PropertyT, forAll, (===))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Slist.Size (Size (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


type Property = PropertyT IO ()

sizeSpec :: Spec
sizeSpec = describe "Size tests" $
    describe "'Num' laws" $ do
        it "Neutrality of 0 over addition" zeroAdditionNeutrality
        it "Commutativity of (+)" additionCommutativity
        it "Associativity of (+)" additionAssotiavity
        it "Neutrality of 1 over multiplication" oneMultiplicationNeutrality
        it "Commutativity of (*)" multiplicationCommutavity
        it "Associativity of (*)" multiplicationAssociativity
        it "Distributivity of (*) with respect to (+)" distributivity
        it "'abs' and 'signum' correspondence" absSignum

zeroAdditionNeutrality :: Property
zeroAdditionNeutrality = hedgehog $ do
    x <- forAll genSize

    fromInteger 0 + x === x
    x + fromInteger 0 === x

additionCommutativity :: Property
additionCommutativity = hedgehog $ do
    x <- forAll genSize
    y <- forAll genSize

    x + y === y + x

additionAssotiavity :: Property
additionAssotiavity = hedgehog $ do
    x <- forAll genSize
    y <- forAll genSize
    z <- forAll genSize

    (x + y) + z === x + (y + z)

oneMultiplicationNeutrality :: Property
oneMultiplicationNeutrality = hedgehog $ do
    x <- forAll genSize

    fromInteger 1 * x === x
    x * fromInteger 1 === x

multiplicationCommutavity :: Property
multiplicationCommutavity = hedgehog $ do
    x <- forAll genSize
    y <- forAll genSize

    x * y === y * x

multiplicationAssociativity :: Property
multiplicationAssociativity = hedgehog $ do
    x <- forAll genSize
    y <- forAll genSize
    z <- forAll genSize

    (x * y) * z === x * (y * z)

distributivity :: Property
distributivity = hedgehog $ do
    x <- forAll genSize
    y <- forAll genSize
    z <- forAll genSize

    x * (y + z) === (x * y) + (x * z)
    (y + z) * x === (y * x) + (z * x)

absSignum :: Property
absSignum = hedgehog $ do
    x <- forAll genSize

    abs x * signum x === x

genSize :: Gen Size
genSize = Gen.frequency
    [ (1, pure Infinity)
    , (9, Size <$> Gen.int (Range.constant 0 maxBound))
    ]
