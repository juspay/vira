module Vira.Stream.LogSpec where

import Test.Hspec
import Lucid
import Vira.Stream.Log (LogChunk(..), logChunkMsg)

spec :: Spec
spec = describe "Vira.Stream.Log" $ do
  describe "logChunkMsg" $ do
    it "preserves newlines in streamed log chunks" $ do
      let logLines = "line1" :| ["line2", "line3"]
      let chunk = Chunk 1 logLines
      let htmlOutput = logChunkMsg chunk
      
      -- The HTML output should contain proper newlines between lines
      -- Convert to Text to inspect the content
      let htmlText = decodeUtf8 htmlOutput
      
      -- The output should contain line1, line2, and line3 with newlines
      htmlText `shouldContain` "line1"
      htmlText `shouldContain` "line2" 
      htmlText `shouldContain` "line3"
      
      -- More importantly, when rendered to raw text, it should have newlines
      -- The unlines function should create: "line1\nline2\nline3\n"
      let expectedText = "line1\nline2\nline3\n"
      htmlText `shouldContain` expectedText

    it "handles single line chunks correctly" $ do
      let chunk = Chunk 1 (one "single-line")
      let htmlOutput = logChunkMsg chunk
      let htmlText = decodeUtf8 htmlOutput
      
      htmlText `shouldContain` "single-line"
      -- Single line should still end with newline
      htmlText `shouldContain` "single-line\n"