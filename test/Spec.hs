import Test.Hspec

import HaskellOne

main :: IO ()
main = hspec $ do
    smooth2Tests
    kLargestTests
    boundedEqTests
    treeBfsTests

smooth2Tests :: Spec
smooth2Tests = 
  describe "Testing smooth2" $ do
    it "on the empty list" $ 
      smooth2 [] `shouldBe` []
    it "on the singleton list" $ do
      smooth2 [1.0] `shouldBe` []
      smooth2 [200] `shouldBe` []
    it "on lists of various lengths" $ do
      smooth2 [1.0, 2.0] `shouldBe` [1.5]
      smooth2 [1..10] `shouldBe` [1.5,2.5..9.5]


kLargestTests :: Spec
kLargestTests =
  describe "Testing kLargest" $ do
    it "on several examples" $ do
      kLargest 2 [5, 8, 10, 1] `shouldBe` [8, 10]
      kLargest 4 [5, 8, 10, 1] `shouldBe` [1, 5, 8, 10]



oneTree :: InfBinTree Int
oneTree = Fork 1 (oneTree) (oneTree)

twoTree :: InfBinTree Int
twoTree = Fork 2 (twoTree) (twoTree)


boundedEqTests :: Spec
boundedEqTests =
  describe "Testing boundedEq" $ do
    it "on trees containing just 1s and 2s" $ do
      boundedEq 0 oneTree twoTree `shouldBe` True
      boundedEq 2 oneTree oneTree `shouldBe` True
      boundedEq 2 oneTree twoTree `shouldBe` False
      boundedEq 4 oneTree oneTree `shouldBe` True
    it "with treeRepeat" $ do
      boundedEq 4 (oneTree) (treeRepeat 1) `shouldBe` True
      boundedEq 5 (oneTree) (treeRepeat 1) `shouldBe` True
    it "with xTree and yTree" $ do
      boundedEq 1 xTree yTree `shouldBe` True
      boundedEq 2 xTree yTree `shouldBe` False
      boundedEq 4 xTree yTree `shouldBe` False

completeTree :: InfBinTree Int
completeTree = go 1
  where
    go n = Fork n (go $ 2 * n) (go $ 2 * n + 1)

treeBfsTests :: Spec
treeBfsTests =
  describe "Testing treeBfs" $ do
    it "on the complete tree" $
      (take 10 $ treeBfs completeTree) `shouldBe` [1..10]