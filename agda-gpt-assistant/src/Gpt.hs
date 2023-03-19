{-# LANGUAGE OverloadedStrings #-}

module Gpt where

import Data.Aeson as A 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process as SP
import Data.List as L

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text as T
import System.Exit
import System.FilePath (splitFileName)

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
interactWithGpt :: String -> IO String
interactWithGpt prompt = do
  manager <- newManager tlsManagerSettings
  request <- return (createGptRequest prompt)
  response <- httpLbs request manager
  -- putStrLn $ "Status: " ++ (show $ responseStatus response)
  return  $ (decodeFunction $ responseBody response)

decodeFunction :: BL.ByteString -> String
decodeFunction r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Nothing"
                     Just x -> retriveMsg x

retriveMsg :: ChatCompletion -> String
retriveMsg cc = content $ message $ Prelude.head $ choices cc
       
    
  

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


tryToCompile :: FilePath -> IO (Maybe String)
tryToCompile fp = do
  let (path, file) =  splitFileName fp
  aReq <- runProcess_ path file
  let ret = case aReq of
              Nothing -> Nothing
              Just re -> Just $ removeSubstring path $ removeSubstring path re
  return ret 

                 
removeSubstring :: String -> String -> String
removeSubstring substr str = go str
  where go [] = []
        go s@(x:xs)
          | substr `L.isPrefixOf` s = L.drop (L.length substr) s
          | otherwise = x : go xs


runProcess_ ::  FilePath -> String -> IO (Maybe String)
runProcess_ pwd afile = do
  let cp = shell $ "/home/kryn/.cabal/bin/agda" ++ " " ++ afile
      ncp = cp { cwd = Just pwd
               , std_out = CreatePipe
               , std_err = CreatePipe
               }            
  (code, output, errorOutput) <- readCreateProcessWithExitCode ncp ""
  let result = case code of
                  ExitSuccess   -> Nothing
                  ExitFailure _ -> Just output
  return result
  

x :: IO ()
x = do
  x <-tryToCompile  "/home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/Ok.agda"
  let s =  case x of
           Nothing -> "OK"
           Just k -> k
  putStrLn s


x2 :: IO ()
x2 = do
  x <-tryToCompile  "/home/kryn/agda-gpt-assistant/agda-gpt-assistant/data/Bad.agda"
  let s =  case x of
           Nothing -> "OK"
           Just k -> k
  putStrLn s
