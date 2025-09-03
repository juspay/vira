module Vira.Stream.LogSpec where

import Data.ByteString.Lazy.Char8 qualified as L8
import Test.Hspec
import Vira.Stream.Log (LogChunk (..), logChunkMsg)

spec :: Spec
spec = describe "Vira.Stream.Log" $ do
  describe "logChunkMsg" $ do
    it "preserves line endings when rendering log chunks" $ do
      let logLines = "Line 1" :| ["Line 2", "Line 3"]
      let chunk = Chunk 1 logLines
      let result = L8.unpack $ logChunkMsg chunk
      -- Should contain newlines between lines for proper display in HTML <pre> tags
      result `shouldContain` "Line 1\nLine 2\nLine 3"

    it "handles single line correctly" $ do
      let logLines = "Single line" :| []
      let chunk = Chunk 1 logLines
      let result = L8.unpack $ logChunkMsg chunk
      result `shouldBe` "Single line"

    it "handles empty lines correctly" $ do
      let logLines = "" :| ["", "Content"]
      let chunk = Chunk 1 logLines
      let result = L8.unpack $ logChunkMsg chunk
      result `shouldContain` "\n\nContent"
