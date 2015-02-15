module Specs
where
import Test.Hspec

main = hspec $ do
    describe "a dummy test" $ do
        it "should fail " $ do
            2+2 `shouldBe` 5

