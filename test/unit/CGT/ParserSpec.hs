module CGT.ParserSpec where

import Test.Hspec
import Test.QuickCheck

import CGT.Parser
import CGT.Values

import Arbitrary

canRead :: GameValue -> Bool
canRead gv = Right gv == (eitherValueParser $ show gv)

spec :: Spec
spec = do
  describe "CGT.Parser" $

    describe "valueParser" $

      it "correctly reads the string form of a GameValue" $ property $ canRead
