-- stack ghc examples/Main.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# language ImportQualifiedPost, BlockArguments, OverloadedStrings #-}
import Control.Applicative.Free
import qualified Text.Regex.Applicative as RE
import Data.Text qualified as T
import Data.GBNF
import System.Environment
import System.IO
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Data.Conduit.SSE
import Data.Conduit
import Data.Conduit.LLM
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import Data.Aeson

main :: IO ()
main = do
  test "Give me a comma-separated list of random integers, no other content."
       (let num = someP [Range '0' '9']
        in (:) <$> num <*> manyR (litR ", " *> num))

  test "Generate a list of first names in a s-expression:"
       (let nam = someP [Range 'a' 'z', Range 'A' 'Z']
        in litR "(" *> ((:) <$> nam <*> manyR (litR " " *> nam)) <* litR ")")

test :: Show a => String -> Ap Rule a -> IO ()
test prompt rules' = do
  prefix <- getEnv "LLAMA_PREFIX"
  let requestBody = object
        [ "messages" .=
            [ object
                [ "role" .= ("user" :: String)
                , "content" .= prompt
                ]
            ]
        , "stream" .= True
        , "grammar" .= rootGBNF rules'
        , "temperature" .= (0.7 :: Double)
        , "n_predict" .= (512 :: Double)
        ]
  req <- parseRequest $ "POST " <> prefix <> "/v1/chat/completions"
  let req' = setRequestHeader "Content-Type" ["application/json"]
           $ setRequestBodyJSON requestBody
           $ req

  hSetBuffering stdout NoBuffering

  putStrLn $ "\nGBNF: " ++ rootGBNF rules'

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
    (RE.match (rulesToRegex rules') $ T.unpack $ output) --
