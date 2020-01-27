
import Test.Hspec
-- import Parser
import Packrat

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "test 01" $ do (evalExpr "+3") `shouldBe` (Just 3.00 :: Maybe Double)
        it "test 02" $ do (evalExpr "3+3") `shouldBe` (Just 6.00 :: Maybe Double)
        it "test 03" $ do (evalExpr "3+3+3+3") `shouldBe` (Just 12.00 :: Maybe Double)
        it "test 04" $ do (evalExpr "3.45+5.72") `shouldBe` (Just 9.17 :: Maybe Double)
        it "test 05" $ do (evalExpr "123+123") `shouldBe` (Just 246.00 :: Maybe Double)
        it "test 06" $ do (evalExpr "-3+3") `shouldBe` (Just 0.00 :: Maybe Double)
        it "test 07" $ do (evalExpr "-3+-3") `shouldBe` (Just (-6.00) :: Maybe Double)
        it "test 08" $ do (evalExpr "-(-5.11+(-2)*((((4-1))^(4-2)/(3+2-3*2))))") `shouldBe` (Just (-12.89) :: Maybe Double)

    describe "Substraction" $ do
        it "test 01" $ do (evalExpr "3-3") `shouldBe` (Just 0.00 :: Maybe Double)
        it "test 01" $ do (evalExpr "3-3-3-3") `shouldBe` (Just (-6.00) :: Maybe Double)
        it "test 02" $ do (evalExpr "12.5-3") `shouldBe` (Just 9.50 :: Maybe Double)
        it "test 03" $ do (evalExpr "3.1-5.7") `shouldBe` (Just (-2.6) :: Maybe Double)
        it "test 04" $ do (evalExpr "123-321") `shouldBe` (Just (-198.00) :: Maybe Double)
        it "test 05" $ do (evalExpr "-3-3") `shouldBe` (Just (-6.00) :: Maybe Double)
        it "test 06" $ do (evalExpr "-3--3") `shouldBe` (Just (0.00) :: Maybe Double)
        it "test 07" $ do (evalExpr "-(-3)") `shouldBe` (Just (3.00) :: Maybe Double)


    describe "Multiplication" $ do
        it "test 01" $ do (evalExpr "3*3") `shouldBe` (Just 9.00 :: Maybe Double)
        it "test 02" $ do (evalExpr "2.5*2") `shouldBe` (Just 5.00 :: Maybe Double)
        it "test 02" $ do (evalExpr "4*0.5") `shouldBe` (Just 2.00 :: Maybe Double)
        it "test 03" $ do (evalExpr "2.5*2.5") `shouldBe` (Just 6.25 :: Maybe Double)
        it "test 04" $ do (evalExpr "3*-3") `shouldBe` (Just (-9.00) :: Maybe Double)
        it "test 05" $ do (evalExpr "-3*-3") `shouldBe` (Just (9.00) :: Maybe Double)
        it "test 06" $ do (evalExpr "123*123") `shouldBe` (Just (15129) :: Maybe Double)
        it "test 07" $ do (evalExpr "3*3*3") `shouldBe` (Just (27) :: Maybe Double)

    describe "Division" $ do
        it "test 01" $ do (evalExpr "5/2") `shouldBe` (Just 2.50 :: Maybe Double)
        it "test 02" $ do (evalExpr "2.5/2.5") `shouldBe` (Just 1.00 :: Maybe Double)
        it "test 03" $ do (evalExpr "2/0.5") `shouldBe` (Just 4.00 :: Maybe Double)
        it "test 04" $ do (evalExpr "16/-4") `shouldBe` (Just (-4.00) :: Maybe Double)
        it "test 05" $ do (evalExpr "9876/6") `shouldBe` (Just (1646) :: Maybe Double)
        it "test 06" $ do (evalExpr "16/2/2/2") `shouldBe` (Just (2) :: Maybe Double)

    describe "Power" $ do
        it "test 01" $ do (evalExpr "5^2") `shouldBe` (Just 25 :: Maybe Double)
        it "test 02" $ do (evalExpr "5^-2") `shouldBe` (Just 0.04 :: Maybe Double)
        it "test 03" $ do (evalExpr "-5^2") `shouldBe` (Just 25 :: Maybe Double)
        it "test 04" $ do (evalExpr "2^8") `shouldBe` (Just (256) :: Maybe Double)
        it "test 05" $ do (evalExpr "2^2^2") `shouldBe` (Just (16) :: Maybe Double)
        it "test 06" $ do (evalExpr "0.5^2") `shouldBe` (Just (0.25) :: Maybe Double)


    describe "parenthesis" $ do
        it "test 01" $ do (evalExpr "(1)") `shouldBe` (Just 1 :: Maybe Double)
        it "test 02" $ do (evalExpr "((((((((((1))))))))))") `shouldBe` (Just 1 :: Maybe Double)
        it "test 03" $ do (evalExpr "(1)+(1)") `shouldBe` (Just 2 :: Maybe Double)


    describe "Priority" $ do
        it "test 01" $ do (evalExpr "2+3*2") `shouldBe` (Just 8 :: Maybe Double)
        it "test 02" $ do (evalExpr "2+3*2^2") `shouldBe` (Just 14 :: Maybe Double)
        it "test 03" $ do (evalExpr "((2+3)*2)^2") `shouldBe` (Just 100 :: Maybe Double)

    describe "Spaces" $ do
        it "test 01" $ do (evalExpr "  3 \t ") `shouldBe` (Just 3 :: Maybe Double)
        it "test 02" $ do (evalExpr "\t  3  + \t 3  \t") `shouldBe` (Just 6 :: Maybe Double)

    describe "Error" $ do
        it "empty" $ do (evalExpr "") `shouldBe` (Nothing :: Maybe Double)
        it "division by 0" $ do (evalExpr "3/0") `shouldBe` (Nothing :: Maybe Double)
        it "no operator" $ do (evalExpr "3 3") `shouldBe` (Nothing :: Maybe Double)
        it "no operande" $ do (evalExpr "3+") `shouldBe` (Nothing :: Maybe Double)
        it "illegal character" $ do (evalExpr "3a") `shouldBe` (Nothing :: Maybe Double)
        it "Missing parenthesis" $ do (evalExpr "(3") `shouldBe` (Nothing :: Maybe Double) --)
        it "Two enchained operators" $ do (evalExpr "(3++3") `shouldBe` (Nothing :: Maybe Double)
