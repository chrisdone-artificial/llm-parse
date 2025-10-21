{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Conduit.LLM
    ( StreamChunk(..)
    , Delta(..)
    , Choice(..)
    , FinishReason(..)
    , llmStreamParser
    ) where

import Data.Conduit
import Data.Conduit.SSE (ServerEvent(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

-- | Represents why the model stopped generating
data FinishReason
    = Stop           -- ^ Natural stop point
    | Length         -- ^ Max tokens reached
    | ContentFilter  -- ^ Content filtered
    | ToolCalls      -- ^ Function/tool call complete
    | FunctionCall   -- ^ Legacy function call (deprecated)
    deriving (Show, Eq, Generic)

instance FromJSON FinishReason where
    parseJSON = withText "FinishReason" $ \t -> case t of
        "stop" -> pure Stop
        "length" -> pure Length
        "content_filter" -> pure ContentFilter
        "tool_calls" -> pure ToolCalls
        "function_call" -> pure FunctionCall
        _ -> fail $ "Unknown finish reason: " ++ T.unpack t

-- | Delta represents incremental content changes
data Delta = Delta
    { deltaRole :: Maybe Text
    , deltaContent :: Maybe Text
    , deltaToolCalls :: Maybe Value  -- Simplified; could be more specific
    , deltaFunctionCall :: Maybe Value
    } deriving (Show, Eq, Generic)

instance FromJSON Delta where
    parseJSON = withObject "Delta" $ \o -> Delta
        <$> o .:? "role"
        <*> o .:? "content"
        <*> o .:? "tool_calls"
        <*> o .:? "function_call"

-- | Individual choice in the streaming response
data Choice = Choice
    { choiceIndex :: Int
    , choiceDelta :: Delta
    , choiceFinishReason :: Maybe FinishReason
    } deriving (Show, Eq, Generic)

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \o -> Choice
        <$> o .: "index"
        <*> o .: "delta"
        <*> o .:? "finish_reason"

-- | Represents a chunk from the OpenAI streaming API
data StreamChunk = StreamChunk
    { chunkId :: Text
    , chunkObject :: Text
    , chunkCreated :: Int
    , chunkModel :: Text
    , chunkChoices :: [Choice]
    } deriving (Show, Eq, Generic)

instance FromJSON StreamChunk where
    parseJSON = withObject "StreamChunk" $ \o -> StreamChunk
        <$> o .: "id"
        <*> o .: "object"
        <*> o .: "created"
        <*> o .: "model"
        <*> o .: "choices"

-- | Conduit that parses OpenAI SSE events into StreamChunks
llmStreamParser :: Monad m => ConduitT ServerEvent StreamChunk m ()
llmStreamParser = awaitForever process
  where
    process event
        | eventData event == "[DONE]" = return ()  -- Stream termination signal
        | otherwise = case eitherDecode (BL.fromStrict $ eventData event) of
            Left err_ -> return ()  -- Skip malformed chunks
            Right chunk -> yield chunk
