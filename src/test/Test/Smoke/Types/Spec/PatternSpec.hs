{-# LANGUAGE OverloadedStrings #-}

module Test.Smoke.Types.Spec.PatternSpec (spec) where

import Test.Hspec
import Test.Smoke

spec :: Spec
spec =
  describe "a pattern" $ do
    it "matches a string" $ do
      let text = "All happy families are alike; each unhappy family is unhappy in its own way."
      let textPattern = "All happy families are alike; each unhappy family is unhappy in its own way."
      text `shouldSatisfy` matches textPattern

    it "matches a substring" $ do
      let text = "When Gregor Samsa woke up one day from troubled dreams, he found himself transformed right there in his bed into some sort of monstrous insect."
      let textPattern = "woke up one day from troubled dreams"
      text `shouldSatisfy` matches textPattern

    it "does not match another string" $ do
      let text = "I am an invisible man."
      let textPattern = "a visible man"
      text `shouldNotSatisfy` matches textPattern

    it "supports regular expression operators" $ do
      let text = "Everything in the world began with a yes."
      let textPattern = "began .* yes"
      text `shouldSatisfy` matches textPattern

    it "matches case-sensitively by default" $ do
      let text = "For a long time, I went to bed early."
      let textPattern = patternWithOptions [] "for a long time"
      text `shouldNotSatisfy` matches textPattern

    it "optionally matches case-insensitively" $ do
      let text = "For a long time, I went to bed early."
      let textPattern = patternWithOptions [CaseInsensitive] "for a long time"
      text `shouldSatisfy` matches textPattern

    it "does not allow `.` to cross multiple lines by default" $ do
      let text = "It was the best of times, it was the worst of times,\nit was the age of wisdom, it was the age of foolishness,\nit was the epoch of belief, it was the epoch of incredulity,"
      let textPattern = patternWithOptions [] "was the best .* was the age"
      text `shouldNotSatisfy` matches textPattern

    it "optionally allows `.` to cross multiple lines" $ do
      let text = "It was the best of times, it was the worst of times,\nit was the age of wisdom, it was the age of foolishness,\nit was the epoch of belief, it was the epoch of incredulity,"
      let textPattern = patternWithOptions [DotAll] "was the best .* was the age"
      text `shouldSatisfy` matches textPattern

    it "does not support comments by default" $ do
      let text = "Call me Ishmael."
      let textPattern = patternWithOptions [] "Call  # verb\nme  # subject\nIshmael  # object"
      text `shouldNotSatisfy` matches textPattern

    it "optionally supports comments, with escaped whitespace" $ do
      let text = "Call me Ishmael."
      let textPattern = patternWithOptions [Comments] "Call  # verb\n\\ me  # subject\n\\ Ishmael  # object"
      text `shouldSatisfy` matches textPattern
