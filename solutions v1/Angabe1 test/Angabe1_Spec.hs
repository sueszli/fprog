import Angabe1 (filtere)

-- cabal/stack install hlint

import Test.Hspec -- cabal/stack install hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Data.Ord
import Data.List

main :: IO ()
main = hspec $ do
    describe "filtere" $ do
        it "returns [] on filtere 4 []" $
            filtere 4 [] `shouldBe` []
        it "returns [42] on filtere 1 [42]" $
            filtere 1 [42] `shouldBe` [42]
        it "returns [] on filtere 2 [42]" $
            filtere 2 [42] `shouldBe` []
        it "returns [3,-12] on filtere 1 [4,-2,5,4,3,-2,5,4,-12]" $
            filtere 1 [4,-2,5,4,3,-2,5,4,-12] `shouldBe` [3,-12]
        it "returns [5,-2] on filtere 2 [4,-2,5,4,3,-2,5,4,-12]" $
            filtere 2 [4,-2,5,4,3,-2,5,4,-12] `shouldBe` [5,-2]
        it "returns [4] on filtere 3 [4,-2,5,4,3,-2,5,4,-12]" $
            filtere 3 [4,-2,5,4,3,-2,5,4,-12] `shouldBe` [4]
        modifyMaxSuccess (const 500) $ it "returns descending lists" $ property prop_filtere_descending
        it "is idempotent on filtere 1" $ property prop_filtere_specialIdempotent

prop_filtere_descending :: Positive Int -> [Int] -> Bool
prop_filtere_descending (Positive n) list = result == sortOn Down result
    where
        result = filtere n list

prop_filtere_specialIdempotent :: [Int] -> Bool
prop_filtere_specialIdempotent list = filtere 1 list == filtere 1 (filtere 1 list)