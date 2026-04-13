{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Pipeline.SubstituteVarsSpec (spec) where

import Test.Hspec
import Vira.CI.Pipeline.Type (substituteVars)

spec :: Spec
spec = describe "substituteVars" $ do
  -- -----------------------------------------------------------------------
  -- Basic substitution
  -- -----------------------------------------------------------------------
  describe "basic substitution" $ do
    it "replaces a single variable" $
      substituteVars [("NAME", "world")] "hello $NAME" `shouldBe` "hello world"

    it "replaces multiple distinct variables" $
      substituteVars [("A", "foo"), ("B", "bar")] "$A and $B"
        `shouldBe` "foo and bar"

    it "replaces the same variable appearing more than once" $
      substituteVars [("X", "42")] "$X + $X = 84"
        `shouldBe` "42 + 42 = 84"

    it "returns empty text when input is empty" $
      substituteVars [("X", "v")] "" `shouldBe` ""

    it "returns the literal when there are no placeholders" $
      substituteVars [("X", "v")] "no vars here" `shouldBe` "no vars here"

    it "works with an empty binding list (erases all vars)" $
      substituteVars [] "$FOO bar" `shouldBe` " bar"

  -- -----------------------------------------------------------------------
  -- Variable name character rules
  -- -----------------------------------------------------------------------
  describe "variable name character rules" $ do
    it "accepts names starting with an uppercase letter" $
      substituteVars [("FOO", "yes")] "$FOO" `shouldBe` "yes"

    it "accepts names starting with a lowercase letter" $
      substituteVars [("foo", "yes")] "$foo" `shouldBe` "yes"

    it "accepts names starting with an underscore" $
      substituteVars [("_BAR", "yes")] "$_BAR" `shouldBe` "yes"

    it "accepts names with digits after the first char" $
      substituteVars [("V1", "ok")] "$V1" `shouldBe` "ok"

    it "stops at the first non-identifier character" $
      substituteVars [("VAR", "v")] "$VAR.suffix" `shouldBe` "v.suffix"

    it "stops at a hyphen (not an identifier char)" $
      substituteVars [("VAR", "v")] "$VAR-rest" `shouldBe` "v-rest"

    it "does not treat digit-start as variable ($1BAD stays literal)" $
      substituteVars [("1BAD", "x")] "$1BAD" `shouldBe` "$1BAD"

  -- -----------------------------------------------------------------------
  -- Bare '$' preservation
  -- -----------------------------------------------------------------------
  describe "bare '$' handling" $ do
    it "preserves a trailing bare '$'" $
      substituteVars [] "price $" `shouldBe` "price $"

    it "preserves '$' followed by a space" $
      substituteVars [] "$ 5" `shouldBe` "$ 5"

    it "preserves '$' followed by another '$'" $
      substituteVars [] "$$" `shouldBe` "$$"

    it "preserves '$' at the start when followed by a digit" $
      substituteVars [] "$9" `shouldBe` "$9"

  -- -----------------------------------------------------------------------
  -- Unknown / missing variables
  -- -----------------------------------------------------------------------
  describe "unknown variables" $ do
    it "erases an unknown variable (replaces with empty string)" $
      substituteVars [] "$UNKNOWN" `shouldBe` ""

    it "erases an unknown variable but keeps surrounding text" $
      substituteVars [] "pre $UNKNOWN post" `shouldBe` "pre  post"

    it "erases only the unknown var, not a known one" $
      substituteVars [("KNOWN", "k")] "$UNKNOWN $KNOWN"
        `shouldBe` " k"

  -- -----------------------------------------------------------------------
  -- No recursive expansion
  -- -----------------------------------------------------------------------
  describe "single-pass (no recursive expansion)" $ do
    it "does not re-substitute a value that itself contains a placeholder" $
      substituteVars [("A", "$B"), ("B", "danger")] "$A"
        `shouldBe` "$B"

    it "does not expand a variable introduced by another substitution" $
      substituteVars
        [("OUTER", "$INNER"), ("INNER", "leaked")]
        "$OUTER"
        `shouldBe` "$INNER"

  -- -----------------------------------------------------------------------
  -- Vira-style real-world patterns
  -- -----------------------------------------------------------------------
  describe "real-world webhook patterns" $ do
    it "substitutes a URL with branch and commit" $
      substituteVars
        [ ("VIRA_BRANCH", "main")
        , ("VIRA_COMMIT_ID", "abc123")
        ]
        "https://api.example.com/deploy?branch=$VIRA_BRANCH&commit=$VIRA_COMMIT_ID"
        `shouldBe` "https://api.example.com/deploy?branch=main&commit=abc123"

    it "substitutes a Bearer token header value" $
      substituteVars [("CI_TOKEN", "s3cr3t")] "Bearer $CI_TOKEN"
        `shouldBe` "Bearer s3cr3t"

    it "substitutes a JSON body template" $
      substituteVars
        [ ("VIRA_BRANCH", "feature/x")
        , ("VIRA_COMMIT_ID", "deadbeef")
        ]
        "{\"branch\":\"$VIRA_BRANCH\",\"commit\":\"$VIRA_COMMIT_ID\"}"
        `shouldBe` "{\"branch\":\"feature/x\",\"commit\":\"deadbeef\"}"

    it "handles a URL where the variable is the entire path component" $
      substituteVars [("REPO", "my-repo")] "https://host/$REPO/hook"
        `shouldBe` "https://host/my-repo/hook"

  -- -----------------------------------------------------------------------
  -- Edge cases
  -- -----------------------------------------------------------------------
  describe "edge cases" $ do
    it "handles a text that is only a variable" $
      substituteVars [("ONLY", "just this")] "$ONLY"
        `shouldBe` "just this"

    it "handles adjacent variables with no separator" $
      substituteVars [("A", "1"), ("B", "2")] "$A$B"
        `shouldBe` "12"

    it "handles a value that is empty string" $
      substituteVars [("EMPTY", "")] "before${EMPTY}after"
        `shouldBe` "before${EMPTY}after"

    it "handles a value that is empty string (non-ident boundary)" $
      substituteVars [("EMPTY", "")] "before $EMPTY after"
        `shouldBe` "before  after"

    it "handles unicode in the literal text (not in var names)" $
      substituteVars [("LANG", "日本語")] "lang=$LANG end"
        `shouldBe` "lang=日本語 end"

    it "handles a very long literal with no variables efficiently" $ do
      let long = mconcat (replicate 10000 "abcdefghij")
      substituteVars [] long `shouldBe` long
