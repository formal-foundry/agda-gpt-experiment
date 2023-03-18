{-# LANGUAGE OverloadedStrings #-}

module Gpt where

import Data.Aeson as A 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- GPT API endpoint
gptApiUrl :: String
-- gptApiUrl = "https://api.openai.com/v1/engines/davinci-codex/completions"
gptApiUrl = "https://api.openai.com/v1/chat/completions"

-- Your API key
apiKey :: B.ByteString
apiKey = "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"

-- Create a request to the GPT API
createGptRequest :: String -> Request
createGptRequest prompt = request
  where
    baseRequest = parseRequest_ gptApiUrl
    request = baseRequest
      { method = "POST"
      , requestHeaders = [ ("Content-Type", "application/json")
                         , ("Authorization", B.concat ["Bearer ", apiKey])
                         ]
      , requestBody = RequestBodyLBS (encodeJson prompt)
      }

-- Encode the prompt as a JSON object
encodePrompt :: String -> BL.ByteString
encodePrompt prompt = encode (A.object ["model" .= prompt, "max_tokens" .= (50 :: Int)])

-- Make a request to the GPT API and print the response
interactWithGpt :: String -> IO ()
interactWithGpt prompt = do
  manager <- newManager tlsManagerSettings
  request <- return (createGptRequest prompt)
  response <- httpLbs request manager
  putStrLn $ "Status: " ++ (show $ responseStatus response)
  putStrLn $ "Response: " ++ (decodeFunction $ responseBody response)

decodeFunction :: BL.ByteString -> String
decodeFunction r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Nothing"
                     Just x -> retriveMsg x

retriveMsg :: ChatCompletion -> String
retriveMsg cc = content $ message $ head $ choices cc
       
    
  

encodeJson :: String -> BL.ByteString
encodeJson msg = encode $
    A.object [ "model" .= ("gpt-3.5-turbo" :: String)
           , "messages" .= [ A.object [ "role" .= ("user" :: String)
                                     , "content" .= msg
                                     ]
                           ]
           ]

data ChatCompletion = ChatCompletion
    { id :: String
    , object :: String
    , created :: Integer
    , model :: String
    , usage :: Usage
    , choices :: [Choice]
    } deriving (Show)

data Usage = Usage
    { prompt_tokens :: Int
    , completion_tokens :: Int
    , total_tokens :: Int
    } deriving (Show)

data Choice = Choice
    { message :: Message
    , finish_reason :: String
    , index :: Int
    } deriving (Show)

data Message = Message
    { role :: String
    , content :: String
    } deriving (Show)

instance FromJSON ChatCompletion where
    parseJSON = withObject "ChatCompletion" $ \v -> ChatCompletion
        <$> v .: "id"
        <*> v .: "object"
        <*> v .: "created"
        <*> v .: "model"
        <*> v .: "usage"
        <*> v .: "choices"

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \v -> Usage
        <$> v .: "prompt_tokens"
        <*> v .: "completion_tokens"
        <*> v .: "total_tokens"

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \v -> Choice
        <$> v .: "message"
        <*> v .: "finish_reason"
        <*> v .: "index"

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "role"
        <*> v .: "content"
