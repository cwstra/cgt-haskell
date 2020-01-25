module Arbitrary where

import Control.Monad
import Data.Ratio

import Data.Hashable
import qualified Data.HashSet as HS
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import CGT.Parser
import CGT.Values

instance Arbitrary Dyadic where
  arbitrary = oneof [arbitraryInt, arbitraryRat]
    where
      arbitraryInt = liftM dyInt arbitrary
      arbitraryRat = do
        num <- liftM (\x -> 2*x + 1) (arbitrary :: Gen Integer)
        den_power <- liftM (\x -> abs x + 1) (arbitrary :: Gen Integer)
        let den = 2 ^ den_power
        return $ dyRat $ num % den

  shrink (DyInt n) = map dyInt $ shrink n
  shrink (DyRat r) = map dyRat [p % q|q <- shrunkenDens, p <- shrunkenNums]
    where
      num = numerator r
      den = denominator r
      pow = (round $ logBase 2 (fromIntegral den)) :: Integer
      shrunkenDens = map (2^) $ filter (>=0) $ shrink pow
      shrunkenNums = shrink num

instance (Arbitrary a, Hashable a, Eq a) => Arbitrary (HS.HashSet a) where
  arbitrary = liftM HS.fromList arbitrary
  shrink m = map HS.fromList $ shrink $ HS.toList m

instance Arbitrary GameValue where
  arbitrary = oneof [arbitraryNumber, arbitraryNimber, arbitraryOffNim, arbitraryRec]
    where
      arbitraryNumber = liftM Number arbitrary
      arbitraryNimber = liftM nimber arbitrary
      arbitraryOffNim = liftM2 offNim arbitrary arbitrary
      arbitraryRec    = sized $ \n ->
        do  [l, r] <- replicateM 2 $ resize (n `div` 2) arbitrary
            return $ game l r
