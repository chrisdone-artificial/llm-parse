-- stack ghc examples/Main.hs
{-# language BlockArguments, OverloadedStrings #-}
import System.IO
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text.IO as T
import Data.Conduit.SSE
import Data.Conduit
import Data.Conduit.LLM
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import Data.Aeson

main = do
  -- Make a request
  let requestBody = object
        [ "messages" .=
            [ object
                [ "role" .= ("user" :: String)
                , "content" .= ("Hello! Can you tell me a joke?" :: String)
                ]
            ]
        , "stream" .= True
        , "temperature" .= (0.7 :: Double)
        ]
  req <- parseRequest "POST http://10.0.1.85:8080/v1/chat/completions"
  let req' = setRequestHeader "Content-Type" ["application/json"]
           $ setRequestBodyJSON requestBody
           $ req


  -- Show one token at a time
  hSetBuffering stdout NoBuffering

  -- Make the request and put the words to stdout
  runConduitRes $ httpSource req' getResponseBody
    .| sseParser           -- ByteString → ServerEvent
    .| llmStreamParser     -- ServerEvent → StreamChunk
    .| CL.mapM_ \chunk ->
        mapM_ (liftIO . T.putStr . fromMaybe "" . deltaContent . choiceDelta)
              (chunkChoices chunk)
