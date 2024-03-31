module Pure.ModuleSpec (spec) where

import Pure.AST (Expr (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec = do
  describe "Show Expr" $ do
    it "shows integers correctly" $ do
      show (Int 0) `shouldBe` "0"
      show (Int 42) `shouldBe` "42"
      show (Int (-123)) `shouldBe` "-123"

    it "shows floats correctly" $ do
      show (Float 0) `shouldBe` "0.0"
      show (Float (-66.6)) `shouldBe` "-66.6"
      show (Float 3.14) `shouldBe` "3.14"

    it "shows strings correctly" $ do
      show (Str "") `shouldBe` "\"\""
      show (Str "hello") `shouldBe` "\"hello\""

    it "shows identifiers correctly" $ do
      show (Id "main") `shouldBe` "main"
      show (Id "main.main") `shouldBe` "main.main"

    it "shows lists correctly" $ do
      show (List [Int 1, Id "main", Float 3.14]) `shouldBe` "[1, main, 3.14]"

    it "shows applications correctly" $ do
      show (App (Id "f") [Lam "a" $ Id "a", Int 0]) `shouldBe` "f (a -> a) 0"

    it "shows ifs correctly" $ do
      show iff `shouldBe` "if f x then if False then 0 else 1 else 42"

    it "shows lambdas correctly" $ do
      show (Lam "a" $ Lam "b" $ Id "a") `shouldBe` "a -> b -> a"
  where
    iff = If (App (Id "f") [Id "x"]) if' (Int 42)
    if' = If (Id "False") (Int 0) (Int 1)
