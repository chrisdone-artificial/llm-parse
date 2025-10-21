{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.LLM
import Data.Conduit.SSE (ServerEvent(..))
import qualified Data.ByteString.Char8 as BS8

-- Helper function to parse ServerEvents into StreamChunks
parseLLMStream :: [ServerEvent] -> IO [StreamChunk]
parseLLMStream events = runConduit $ CL.sourceList events .| llmStreamParser .| CL.consume

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "llmStreamParser" $ do

    it "parses a simple content chunk" $ do
      let event = ServerEvent
            { eventType = Nothing
            , eventId = Nothing
            , eventData = BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-123\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1234567890,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": ["
                , "    {"
                , "      \"index\": 0,"
                , "      \"delta\": {"
                , "        \"content\": \"Hello\""
                , "      },"
                , "      \"finish_reason\": null"
                , "    }"
                , "  ]"
                , "}"
                ]
            , eventRetry = Nothing
            }
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] ->
          chunkId chunk == "chatcmpl-123" &&
          chunkModel chunk == "gpt-4" &&
          case chunkChoices chunk of
            [choice] -> deltaContent (choiceDelta choice) == Just "Hello"
            _ -> False
        _ -> False

    it "parses a chunk with role" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-456\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-3.5-turbo\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {"
            , "        \"role\": \"assistant\""
            , "      },"
            , "      \"finish_reason\": null"
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] ->
            deltaRole (choiceDelta choice) == Just "assistant" &&
            deltaContent (choiceDelta choice) == Nothing
          _ -> False
        _ -> False

    it "parses a chunk with finish_reason stop" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-789\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-4\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {},"
            , "      \"finish_reason\": \"stop\""
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] -> choiceFinishReason choice == Just Stop
          _ -> False
        _ -> False

    it "parses a chunk with finish_reason length" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-abc\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-4\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {},"
            , "      \"finish_reason\": \"length\""
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] -> choiceFinishReason choice == Just Length
          _ -> False
        _ -> False

    it "ignores [DONE] termination event" $ do
      let events =
            [ ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-123\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1234567890,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": ["
                , "    {"
                , "      \"index\": 0,"
                , "      \"delta\": {\"content\": \"Done\"},"
                , "      \"finish_reason\": null"
                , "    }"
                , "  ]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing "[DONE]" Nothing
            ]
      result <- parseLLMStream events
      length result `shouldBe` 1

    it "handles multiple chunks in sequence" $ do
      let events =
            [ ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-1\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1234567890,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \"Hello\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-1\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1234567890,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \" world\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-1\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1234567890,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {}, \"finish_reason\": \"stop\"}]"
                , "}"
                ]) Nothing
            ]
      result <- parseLLMStream events
      result `shouldSatisfy` \case
        [c1, c2, c3] ->
          (case chunkChoices c1 of
            [choice] -> deltaContent (choiceDelta choice) == Just "Hello"
            _ -> False) &&
          (case chunkChoices c2 of
            [choice] -> deltaContent (choiceDelta choice) == Just " world"
            _ -> False) &&
          (case chunkChoices c3 of
            [choice] -> choiceFinishReason choice == Just Stop
            _ -> False)
        _ -> False

    it "skips malformed JSON events" $ do
      let events =
            [ ServerEvent Nothing Nothing "not valid json" Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-123\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1234567890,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \"Valid\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            ]
      result <- parseLLMStream events
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] -> deltaContent (choiceDelta choice) == Just "Valid"
          _ -> False
        _ -> False

    it "handles empty delta object" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-empty\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-4\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {},"
            , "      \"finish_reason\": null"
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] ->
            let delta = choiceDelta choice
            in deltaRole delta == Nothing && deltaContent delta == Nothing
          _ -> False
        _ -> False

    it "parses compact JSON without whitespace" $ do
      let event = ServerEvent Nothing Nothing
            "{\"id\":\"chatcmpl-compact\",\"object\":\"chat.completion.chunk\",\"created\":1234567890,\"model\":\"gpt-4\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Compact\"},\"finish_reason\":null}]}"
            Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] -> deltaContent (choiceDelta choice) == Just "Compact"
          _ -> False
        _ -> False

    it "handles multiple choices in a single chunk" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-multi\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-4\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {\"content\": \"First\"},"
            , "      \"finish_reason\": null"
            , "    },"
            , "    {"
            , "      \"index\": 1,"
            , "      \"delta\": {\"content\": \"Second\"},"
            , "      \"finish_reason\": null"
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [c1, c2] ->
            choiceIndex c1 == 0 &&
            choiceIndex c2 == 1 &&
            deltaContent (choiceDelta c1) == Just "First" &&
            deltaContent (choiceDelta c2) == Just "Second"
          _ -> False
        _ -> False

    it "handles content_filter finish reason" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-filtered\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-4\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {},"
            , "      \"finish_reason\": \"content_filter\""
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] -> choiceFinishReason choice == Just ContentFilter
          _ -> False
        _ -> False

    it "handles tool_calls finish reason" $ do
      let event = ServerEvent Nothing Nothing (BS8.pack $ unlines
            [ "{"
            , "  \"id\": \"chatcmpl-tool\","
            , "  \"object\": \"chat.completion.chunk\","
            , "  \"created\": 1234567890,"
            , "  \"model\": \"gpt-4\","
            , "  \"choices\": ["
            , "    {"
            , "      \"index\": 0,"
            , "      \"delta\": {},"
            , "      \"finish_reason\": \"tool_calls\""
            , "    }"
            , "  ]"
            , "}"
            ]) Nothing
      result <- parseLLMStream [event]
      result `shouldSatisfy` \case
        [chunk] -> case chunkChoices chunk of
          [choice] -> choiceFinishReason choice == Just ToolCalls
          _ -> False
        _ -> False

    it "handles real-world streaming scenario" $ do
      let events =
            [ ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-real\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1677652288,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"role\": \"assistant\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-real\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1677652288,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \"The\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-real\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1677652288,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \" answer\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-real\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1677652288,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \" is\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-real\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1677652288,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {\"content\": \" 42\"}, \"finish_reason\": null}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing (BS8.pack $ unlines
                [ "{"
                , "  \"id\": \"chatcmpl-real\","
                , "  \"object\": \"chat.completion.chunk\","
                , "  \"created\": 1677652288,"
                , "  \"model\": \"gpt-4\","
                , "  \"choices\": [{\"index\": 0, \"delta\": {}, \"finish_reason\": \"stop\"}]"
                , "}"
                ]) Nothing
            , ServerEvent Nothing Nothing "[DONE]" Nothing
            ]
      result <- parseLLMStream events
      result `shouldSatisfy` \case
        [c1, c2, c3, c4, c5, c6] ->
          (case chunkChoices c1 of
            [choice] -> deltaRole (choiceDelta choice) == Just "assistant"
            _ -> False) &&
          (case chunkChoices c2 of
            [choice] -> deltaContent (choiceDelta choice) == Just "The"
            _ -> False) &&
          (case chunkChoices c3 of
            [choice] -> deltaContent (choiceDelta choice) == Just " answer"
            _ -> False) &&
          (case chunkChoices c4 of
            [choice] -> deltaContent (choiceDelta choice) == Just " is"
            _ -> False) &&
          (case chunkChoices c5 of
            [choice] -> deltaContent (choiceDelta choice) == Just " 42"
            _ -> False) &&
          (case chunkChoices c6 of
            [choice] -> choiceFinishReason choice == Just Stop
            _ -> False)
        _ -> False
