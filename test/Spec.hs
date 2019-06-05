import Test.Hspec
import Cipher

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main = hspec $ do
  describe "Caesar" $ do
    it "should rotate abc to bcd" $ do
      caesar 1 "abc" `shouldBe` "bcd"
    it "should rotate amz to cob" $ do
      caesar 2 "amz" `shouldBe` "cob"
    it "should rotate jeff to xyyz" $ do
      caesar 100 "jeff" `shouldBe` "fabb"

  describe "Un-Caesar" $ do
    it "should rotate bcd to abc" $ do
      unCaesar 1 "bcd" `shouldBe` "abc"
    it "should rotate cob to amz" $ do
      unCaesar 2 "cob" `shouldBe` "amz"
    -- it "should rotate fabb to jeff" $ do
      -- unCaesar 100 "fabb" `shouldBe` "jeff"
    it "should rotate jeff to jeff" $ do
     unCaesar 10 (caesar 10 "jeff") `shouldBe` "jeff"

  describe "shiftRight" $ do
    it "should rotate 'a' to 'b'" $ do
      shiftRight 1 'a' `shouldBe` 'b'
    it "should rotate 'a' to 'c'" $ do
      shiftRight 2 'a' `shouldBe` 'c'
    it "should rotate 'z' to 'a'" $ do
      shiftRight 1 'z' `shouldBe` 'a'
    it "should rotate 'z' to 'e'" $ do
      shiftRight 5 'z' `shouldBe` 'e'
    it "should rotate 'z' to 'a'" $ do
      shiftRight 27 'z' `shouldBe` 'a'
    it "should rotate 'j' to 'b'" $ do
      shiftRight 148 'j' `shouldBe` 'b'

  describe "shiftLeft" $ do
    it "should rotate 'b' to 'a'" $ do
      shiftLeft 1 'b' `shouldBe` 'a'
    it "should rotate 'c' to 'a'" $ do
      shiftLeft 2 'c' `shouldBe` 'a'
    it "should rotate 'z' to 'y'" $ do
      shiftLeft 1 'z' `shouldBe` 'y'
    it "should rotate 'a' to 'y'" $ do
      shiftLeft 2 'a' `shouldBe` 'y'
    it "should rotate 'a' to 'z'" $ do
      shiftLeft 27 'a' `shouldBe` 'z'
    -- it "should rotate 'j' to 'b'" $ do
    --   shiftLeft 148 'j' `shouldBe` 'b'