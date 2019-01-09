import Palindromes
import Test.Hspec
import Test.QuickCheck

unique :: [String] -> Bool
unique []     = True
unique (x:xs) = (not $ elem x xs) && unique xs

main :: IO ()
main = hspec $ do
         describe "palindromes5" $ do
           it "finds the correct amount of palindromes" $ do
             (length $ palindromes5 ['a'..'z']) `shouldBe` 26^3
           it "finds unique palindromes" $ do
             (unique $ palindromes5 ['a'..'z']) `shouldBe` True
