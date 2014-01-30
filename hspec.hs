import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.Packed.Syntax.Internal
import Language.Haskell.TH
import Text.Printf
import Data.List

main = hspec $ do
  describe "matListExp" $ do
    it "tabs before semicolons" $
      matListExp "a\t;b" `shouldBe`
        Right (1,2,[[VarE (mkName "a")],
                    [VarE (mkName "b")]])
    it "tabs after semicolons" $ 
      matListExp "a;\tb;\tc" `shouldBe`
        Right (1,3,[[VarE (mkName "a")],
                    [VarE (mkName "b")],
                    [VarE (mkName "c")]])
    modifyMaxSize (\n -> n `div` 10) $
     it "inverts a pretty-printer" $ property $ \(Positive nrow) (Positive ncol) ->
        matListExp (ppMatListExp nrow ncol) `shouldBe`
            Right(ncol,nrow,expectedMatList nrow ncol)

ppMatListExp :: Int -> Int -> String
ppMatListExp nrow ncol = intercalate ";" [ intercalate "," xs |
    i <- [1 .. nrow],
    let xs :: [String]
        xs = map (printf "x%d_%d" i) [1 .. ncol] ]

expectedMatList :: Int -> Int -> [[Exp]]
expectedMatList  nrow ncol = [ map (VarE . mkName . printf "x%d_%d" i) [1 .. ncol]
    | i <- [1 .. nrow]]
