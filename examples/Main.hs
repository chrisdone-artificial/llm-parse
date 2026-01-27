-- stack ghc examples/Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# language ImportQualifiedPost, BlockArguments, OverloadedStrings #-}
import qualified Text.Regex.Applicative as RE
import Data.Text qualified as T
import Data.GBNF
import System.Environment
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
  prefix <- getEnv "LLAMA_PREFIX"

  let rules = (:) <$> num <*> manyR (litR ", " *> num)
        where num = someP [Range '0' '9']
  let prompt :: String =
        "Give me a comma-separated list of integers, no other content."

  let requestBody = object
        [ "messages" .=
            [ object
                [ "role" .= ("user" :: String)
                , "content" .= prompt
                ]
            ]
        , "stream" .= True
        , "grammar" .= rootGBNF rules
        , "temperature" .= (0.7 :: Double)
        ]
  req <- parseRequest $ "POST " <> prefix <> "/v1/chat/completions"
  let req' = setRequestHeader "Content-Type" ["application/json"]
           $ setRequestBodyJSON requestBody
           $ req

  hSetBuffering stdout NoBuffering

  putStrLn $ "\nGBNF: " ++ rootGBNF rules

  putStr "\nStream: "
  out <- runConduitRes $ httpSource req' getResponseBody
    .| sseParser
    .| llmStreamParser
    .| CL.concatMapM (\chunk ->
        mapM (\c -> do
                let out = deltaContent . choiceDelta $ c
                maybe (pure ()) (liftIO . T.putStr) out
                pure $ maybe [] pure out)
              (chunkChoices chunk))
     .| CL.consume

  let output = T.concat $ map T.concat out
  putStrLn $ "\n\nParse: " ++ show
    (RE.match (rulesToRegex rules) $ T.unpack $ output) --
