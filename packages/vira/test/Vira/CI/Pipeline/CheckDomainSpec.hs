{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Pipeline.CheckDomainSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec
import Vira.CI.Pipeline.Implementation (checkDomain, isIpLiteral, isLoopbackHost, sanitiseHeaderName, sanitiseHeaderValue)

{- | Convenience wrapper: in tests the resolved URL and template URL are the same
(no variable substitution takes place), so we pass the URL for both arguments.
-}
cd :: Maybe (Set.Set T.Text) -> T.Text -> Either T.Text ()
cd domains url = checkDomain domains url url

spec :: Spec
spec = describe "checkDomain" $ do
  -- -----------------------------------------------------------------------
  -- Nothing = env var absent → deny-by-default
  -- -----------------------------------------------------------------------
  describe "VIRA_WEBHOOK_ALLOWED_DOMAINS not set (Nothing)" $ do
    it "blocks any https URL when the env var is absent" $
      cd Nothing "https://hooks.example.com/deploy"
        `shouldSatisfy` isLeft

    it "error message tells the operator what to do" $ do
      case cd Nothing "https://example.com/hook" of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "VIRA_WEBHOOK_ALLOWED_DOMAINS"
        Right _ -> expectationFailure "Expected Left but got Right"

  -- -----------------------------------------------------------------------
  -- Just empty set → every host is rejected (empty allowlist = deny all)
  -- -----------------------------------------------------------------------
  describe "VIRA_WEBHOOK_ALLOWED_DOMAINS set to empty string (Just empty set)" $ do
    it "blocks any URL when the set is empty" $
      cd (Just Set.empty) "https://hooks.example.com/deploy"
        `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- Non-empty allowlist — permit listed hosts
  -- -----------------------------------------------------------------------
  describe "non-empty allowlist — permitted hosts" $ do
    it "permits a URL whose host exactly matches an allowlisted domain" $
      cd (Just $ Set.fromList ["hooks.example.com"]) "https://hooks.example.com/deploy"
        `shouldBe` Right ()

    it "permits a URL with a path and query string" $
      cd (Just $ Set.fromList ["api.example.com"]) "https://api.example.com/v1/notify?token=x"
        `shouldBe` Right ()

    it "permits when the allowlist has multiple entries and host matches one" $
      cd (Just $ Set.fromList ["a.com", "b.com", "hooks.example.com"]) "https://b.com/hook"
        `shouldBe` Right ()

    it "rejects an http (non-TLS) URL even when host is in the allowlist" $
      cd (Just $ Set.fromList ["internal.corp"]) "http://internal.corp/webhook"
        `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- Scheme validation: only https is permitted
  -- -----------------------------------------------------------------------
  describe "scheme validation" $ do
    it "rejects a file:// URL even when host would otherwise be allowed" $
      cd (Just $ Set.fromList ["example.com"]) "file:///etc/passwd"
        `shouldSatisfy` isLeft

    it "rejects an ftp:// URL" $
      cd (Just $ Set.fromList ["example.com"]) "ftp://example.com/file"
        `shouldSatisfy` isLeft

    it "rejects an http:// URL even when host is in the allowlist" $
      cd (Just $ Set.fromList ["example.com"]) "http://example.com/hook"
        `shouldSatisfy` isLeft

    it "error message mentions the rejected scheme" $ do
      case cd (Just $ Set.fromList ["example.com"]) "ftp://example.com/file" of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "ftp"
        Right _ -> expectationFailure "Expected Left but got Right"

  -- -----------------------------------------------------------------------
  -- Non-empty allowlist — reject unlisted hosts
  -- -----------------------------------------------------------------------
  describe "non-empty allowlist — rejected hosts" $ do
    it "rejects a URL whose host is not in the allowlist" $
      cd (Just $ Set.fromList ["hooks.example.com"]) "https://evil.com/exfil"
        `shouldSatisfy` isLeft

    it "rejects when host doesn't match any entry in a multi-entry allowlist" $
      cd (Just $ Set.fromList ["a.com", "b.com"]) "https://c.com/hook"
        `shouldSatisfy` isLeft

    it "error message mentions the offending host" $ do
      case cd (Just $ Set.fromList ["allowed.com"]) "https://notallowed.com/hook" of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "notallowed.com"
        Right _ -> expectationFailure "Expected Left but got Right"

    it "error message mentions VIRA_WEBHOOK_ALLOWED_DOMAINS" $ do
      case cd (Just $ Set.fromList ["allowed.com"]) "https://other.com/hook" of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "VIRA_WEBHOOK_ALLOWED_DOMAINS"
        Right _ -> expectationFailure "Expected Left but got Right"

  -- -----------------------------------------------------------------------
  -- Host matching is exact (no subdomain wildcards)
  -- -----------------------------------------------------------------------
  describe "host matching is exact (no implicit wildcard)" $ do
    it "does not treat a listed parent domain as matching a subdomain" $
      cd (Just $ Set.fromList ["example.com"]) "https://sub.example.com/hook"
        `shouldSatisfy` isLeft

    it "does not treat a listed subdomain as matching the parent" $
      cd (Just $ Set.fromList ["hooks.example.com"]) "https://example.com/hook"
        `shouldSatisfy` isLeft

    it "does not treat a prefix match as a full match" $
      cd (Just $ Set.fromList ["example.com"]) "https://example.com.evil.org/hook"
        `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- Edge cases
  -- -----------------------------------------------------------------------
  describe "edge cases" $ do
    it "rejects an invalid URL even when allowlist is configured" $
      cd (Just $ Set.fromList ["example.com"]) "not-a-url"
        `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- Loopback blocking (unconditional, regardless of allowlist)
  -- -----------------------------------------------------------------------
  describe "loopback addresses are unconditionally blocked" $ do
    it "blocks localhost even if listed in allowlist" $
      cd (Just $ Set.fromList ["localhost"]) "https://localhost/admin"
        `shouldSatisfy` isLeft

    it "blocks 127.0.0.1 even if listed in allowlist" $
      cd (Just $ Set.fromList ["127.0.0.1"]) "https://127.0.0.1/secret"
        `shouldSatisfy` isLeft

    it "blocks 127.x.x.x range (e.g. 127.0.0.2)" $
      cd (Just $ Set.fromList ["127.0.0.2"]) "https://127.0.0.2/hook"
        `shouldSatisfy` isLeft

    it "blocks ::1 (IPv6 loopback) even if listed in allowlist" $
      cd (Just $ Set.fromList ["[::1]"]) "https://[::1]/hook"
        `shouldSatisfy` isLeft

    it "blocks 0.0.0.0 even if listed in allowlist" $
      cd (Just $ Set.fromList ["0.0.0.0"]) "https://0.0.0.0/hook"
        `shouldSatisfy` isLeft

    it "error message does not expose the resolved URL (uses template)" $ do
      -- When resolvedUrl == templateUrl the message should still refer to template
      case cd (Just $ Set.fromList ["localhost"]) "https://localhost/hook" of
        Left msg -> msg `shouldSatisfy` T.isInfixOf "template:"
        Right _ -> expectationFailure "Expected Left but got Right"

  -- -----------------------------------------------------------------------
  -- IP address literal blocking (unconditional)
  -- -----------------------------------------------------------------------
  describe "IP address literals are unconditionally blocked" $ do
    it "blocks a public IPv4 address even if listed in allowlist" $
      cd (Just $ Set.fromList ["1.2.3.4"]) "https://1.2.3.4/hook"
        `shouldSatisfy` isLeft

    it "blocks the AWS metadata IP 169.254.169.254" $
      cd (Just $ Set.fromList ["169.254.169.254"]) "https://169.254.169.254/latest/meta-data"
        `shouldSatisfy` isLeft

    it "blocks a private IPv4 range 10.x address" $
      cd (Just $ Set.fromList ["10.0.0.1"]) "https://10.0.0.1/internal"
        `shouldSatisfy` isLeft

    it "blocks an IPv6 address literal" $
      cd (Just $ Set.fromList ["[2001:db8::1]"]) "https://[2001:db8::1]/hook"
        `shouldSatisfy` isLeft

    it "does not block a normal hostname that happens to contain digits" $
      cd (Just $ Set.fromList ["api2.example.com"]) "https://api2.example.com/hook"
        `shouldBe` Right ()

  -- -----------------------------------------------------------------------
  -- isLoopbackHost unit tests
  -- -----------------------------------------------------------------------
  describe "isLoopbackHost" $ do
    it "detects localhost" $ isLoopbackHost "localhost" `shouldBe` True
    it "detects 127.0.0.1" $ isLoopbackHost "127.0.0.1" `shouldBe` True
    it "detects 127.0.0.2" $ isLoopbackHost "127.0.0.2" `shouldBe` True
    it "detects ::1" $ isLoopbackHost "::1" `shouldBe` True
    it "detects 0.0.0.0" $ isLoopbackHost "0.0.0.0" `shouldBe` True
    it "does not flag a normal domain" $ isLoopbackHost "example.com" `shouldBe` False
    it "does not flag a public IP" $ isLoopbackHost "1.2.3.4" `shouldBe` False

  -- -----------------------------------------------------------------------
  -- isIpLiteral unit tests
  -- -----------------------------------------------------------------------
  describe "isIpLiteral" $ do
    it "detects an IPv4 address" $ isIpLiteral "1.2.3.4" `shouldBe` True
    it "detects 169.254.169.254" $ isIpLiteral "169.254.169.254" `shouldBe` True
    it "detects an IPv6 literal in brackets" $ isIpLiteral "[::1]" `shouldBe` True
    it "detects a bracketed IPv6 address" $ isIpLiteral "[2001:db8::1]" `shouldBe` True
    it "does not flag a normal hostname" $ isIpLiteral "example.com" `shouldBe` False
    it "does not flag a hostname with digits" $ isIpLiteral "api2.example.com" `shouldBe` False
    it "does not flag a hostname that starts with digits" $ isIpLiteral "123abc.com" `shouldBe` False

  -- -----------------------------------------------------------------------
  -- URL with port — host matching ignores the port
  -- -----------------------------------------------------------------------
  describe "URLs with ports" $ do
    it "permits a URL with an explicit port when the host is allowlisted" $
      cd (Just $ Set.fromList ["example.com"]) "https://example.com:8443/hook"
        `shouldBe` Right ()

    it "still rejects a URL with a port when the host is not allowlisted" $
      cd (Just $ Set.fromList ["other.com"]) "https://example.com:8443/hook"
        `shouldSatisfy` isLeft

    it "still blocks loopback even with a non-default port" $
      cd (Just $ Set.fromList ["127.0.0.1"]) "https://127.0.0.1:8080/admin"
        `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- sanitiseHeaderName — RFC 7230 token characters only
  -- -----------------------------------------------------------------------
  describe "sanitiseHeaderName" $ do
    it "passes through a valid header name unchanged" $
      sanitiseHeaderName "Content-Type" `shouldBe` "Content-Type"

    it "passes through alphanumeric characters" $
      sanitiseHeaderName "X-Custom-Header123" `shouldBe` "X-Custom-Header123"

    it "strips CRLF injection from a header name" $
      sanitiseHeaderName "X-Foo\r\nX-Injected" `shouldBe` "X-FooX-Injected"

    it "strips a null byte from a header name" $
      sanitiseHeaderName "X-Foo\0Bar" `shouldBe` "X-FooBar"

    it "strips spaces from a header name (spaces are not RFC 7230 token chars)" $
      sanitiseHeaderName "X Foo" `shouldBe` "XFoo"

    it "strips a colon from a header name" $
      sanitiseHeaderName "X-Foo: Bar" `shouldBe` "X-FooBar"

    it "preserves RFC 7230 special token characters (!#$%&'*+-.^_`|~)" $
      sanitiseHeaderName "X-Header!#$" `shouldBe` "X-Header!#$"

  -- -----------------------------------------------------------------------
  -- sanitiseHeaderValue — strips control characters only
  -- -----------------------------------------------------------------------
  describe "sanitiseHeaderValue" $ do
    it "passes through a valid header value unchanged" $
      sanitiseHeaderValue "application/json" `shouldBe` "application/json"

    it "strips carriage return from a header value" $
      sanitiseHeaderValue "value\r\nextra" `shouldBe` "valueextra"

    it "strips null byte from a header value" $
      sanitiseHeaderValue "val\0ue" `shouldBe` "value"

    it "preserves spaces in a header value" $
      sanitiseHeaderValue "Bearer my-token" `shouldBe` "Bearer my-token"
