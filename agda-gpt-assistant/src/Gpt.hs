{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gpt where

import Types 
import Extra


import Control.Monad.Trans.RWS 
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import Data.Aeson as A 
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import System.Process as SP
import System.Console.ANSI
import System.Exit
import System.FilePath (splitFileName)
import System.Directory

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)




decodeRes :: BL.ByteString -> String
decodeRes r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Somethig went wrong with GPT api request.."
                     Just x -> content $ message $ Prelude.head $ choices x

genJsonReq ::String -> [Message] -> BL.ByteString
genJsonReq model messages =
  encode $ A.object ["model" .=model, "messages" .= messages]


-- Create a request to the GPT API
createGptRequest :: String -> [Message] -> String -> Request
createGptRequest model prompt key = request
  where
    apiKey_ = B.pack key 
    baseRequest = parseRequest_ "https://api.openai.com/v1/chat/completions"
    request = baseRequest
      { method = "POST"
      , requestHeaders = [ ("Content-Type", "application/json")
                         , ("Authorization", B.concat ["Bearer ", apiKey_])
                         ]
      , requestBody = RequestBodyLBS (genJsonReq model prompt)
      }



gptConv ::  String -> [Message] -> String -> OperationMode -> IO (String, String)
gptConv model prompt key oM= do
  let rprompt = L.reverse prompt 

  manager <- newManager tlsManagerSettings
  let reqb = createGptRequest model rprompt key
  request <- return (createGptRequest model rprompt key)

  response <- httpLbs reqb manager

  case oM of
    PrettyMode -> return ()
    DebugMode -> do
      setSGR [(SetColor Foreground Dull Yellow)]
      putStrLn $ "\n\n\n" ++ show reqb ++ "\n\n\n\n\n"
      putStrLn $ show $ genJsonReq model rprompt      
      putStrLn $ show $  response

      setSGR [Reset]
  return ()
  return $  (plainCode  (decodeRes $ responseBody response),
            ( decodeRes $ responseBody response))


plainCode :: String ->  String
plainCode res =
  let exR = extractCode  res
  in
  case exR of
    [] -> "EMPTY!!!" --  change into Maybe String 
    _ -> rmSubS "Agda" (rmSubS "agda" (L.concat $ exR))


extractCode :: String -> [String]
extractCode str = extractCodeBlocks' str []
  where extractCodeBlocks' [] acc = acc
        extractCodeBlocks' xs acc = let block = takeCodeBlock xs
                                    in case block of
                                      Nothing -> extractCodeBlocks' (L.drop 1 xs) acc
                                      Just (code, rest) -> extractCodeBlocks' rest (acc ++ [code])

        takeCodeBlock :: String -> Maybe (String, String)
        takeCodeBlock xs = if L.take 3 xs == "```"
                           then let (block, rest) = L.span (/= '`') (L.drop 3 xs)
                                in Just (block, L.drop 3 rest)
                           else Nothing 



tryToCompile :: String -> IO (Maybe String)
tryToCompile fp = do
  let (path, file) =  splitFileName fp
  aReq <- runProcess_ path file
  case aReq of
    Nothing -> return Nothing
    Just re -> do
                return $ Just $ replaceStringLoop path "" re
      
runProcess_ ::  FilePath -> String -> IO (Maybe String)
runProcess_ pwd afile = do
  let cp = shell $ "agda" ++ " " ++ afile
      ncp = cp { cwd = Just pwd
               , std_out = CreatePipe
               , std_err = CreatePipe
               }            
  (code, output, errorOutput) <- readCreateProcessWithExitCode ncp ""
  let result = case code of
                  ExitSuccess   -> Nothing
                  ExitFailure _ -> Just output
  return result
  
-- -----------------------------------------------

debugMode :: AGMonad (Maybe String)
debugMode = do
  env <- ask
  state <- get
  let model = gptModel env
      key = apiKey env
      mode = operationMode env
      dir = dirName env
      sl = L.length state
      agdafile =  (agdaFile env)
      a_log = (dir++"/agda.log")
      gA_log = (dir++"/all_gpt.log")
      gC_log = (dir++"/code_gpt.log")
      r = "REASPONSE\n\n"
      p = "PROMPT\n\n"
      at_info = "\n\n ############## Attempt number:  " ++ show (sl+1) ++ "  ##############\n\n"
      firstPrompt = Message {role = "system", content = "You are a helpful assistant."}
  case mode of
    PrettyMode -> do
      liftIO $ setCursorPosition 0 0
      liftIO clearScreen
    DebugMode -> do
      liftIO $ return ()
      
  liftIO $ cPrint ("\n\n ############## Attempt number:  " ++ show (sl+1) ++ "  ##############\n\n" )  Cyan 
      
  if sl == 0
  then
    do
      aF_content <- liftIO $ readFile agdafile
      liftIO $ appendFile a_log  aF_content
      fcon <- liftIO $ fConvInput env
      liftIO $ cPrint "The following prompt has been sent to GPT chat\n\n" Yellow
      liftIO $ putStrLn $ fcon ++ "\n\n"
      liftIO $ appendFile gC_log  at_info
      liftIO $ appendFile gA_log  at_info
      liftIO $ appendFile gA_log ("\n\n" ++ fcon)
      let promptReq = Message {role = "user", content = fcon}
      answareFromGPT <- liftIO $ gptConv model [promptReq, firstPrompt] key mode
      liftIO $ appendFile gC_log (p++(fcon))
      liftIO $ appendFile gC_log  (r++(fst answareFromGPT))
      -- liftIO $ appendFile  ("\n\n" ++ "c")      
      let promptRes = Message {role = "assistant" , content = (snd answareFromGPT)}
      liftIO $ cPrint "The following GPT chat reasponse was received\n\n" Yellow
      liftIO $ appendFile agdafile (fst answareFromGPT)
      newAfile <- liftIO $ readFile agdafile
      case mode of
        DebugMode -> do
          liftIO $ putStrLn $ snd answareFromGPT ++ "\n\n"
          liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
          liftIO $ putStrLn "Code Only"
          liftIO $ setSGR [Reset]
          liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"

        PrettyMode -> do  
          liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"
          

      compiler <- liftIO $ tryToCompile agdafile
      let newState = (createConvPart fcon answareFromGPT newAfile compiler [ promptRes, promptReq, firstPrompt]  : state)

      put newState
      liftIO $ cPrint "New agda file, with GTP answare \n\n" Magenta
      liftIO $ putStrLn $ newAfile ++ "\n\n"

      case compiler of
        Nothing -> do
                   return Nothing
        Just x -> do
                  liftIO $ cPrint ("The agda compiler response with the following errors\n\n" ++ x) Red
                  return (Just x)

-- __________ >0

  else 
    do
      rcon <- liftIO $ rConvInput  (current_agad_file (L.head state))  env (fromJust(agda_res (L.head state)))
      liftIO $ cPrint "The following prompt has been sent to GPT chat\n\n" Yellow
      liftIO $ putStrLn $ rcon ++ "\n\n"
      let rPromptReq = Message {role =  "user", content = rcon}
          sPrompt =  promptL (L.head state) 
      answareFromGPT <- liftIO $ gptConv model (rPromptReq : sPrompt) key  mode

      let rPromptRes = Message {role = "assistant", content = (snd answareFromGPT)}
      liftIO $ cPrint "The following GPT chat reasponse  was received\n\n" Yellow 
      case mode of
        DebugMode -> do
          liftIO $ putStrLn $ snd answareFromGPT ++ "\n\n"
          liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
          liftIO $ putStrLn "Code Only"
          liftIO $ setSGR [Reset]
          liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"

        PrettyMode -> do  
          liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"
      case (fst answareFromGPT) of
        "EMPTY!!!" -> do
           liftIO $ cPrint "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" Red 
           liftIO $ putStrLn "chatGPT hasn't reasponse code"
           -- liftIO $ appendFile agdafile (fst answareFromGPT)
        _ -> do 
          liftIO $ rmAFile env
          liftIO $ copyFile (orgAgdaF env) (agdaFile env)
          liftIO $ appendFile agdafile (fst answareFromGPT)
         

      newAfile <- liftIO $ readFile agdafile
      compiler <- liftIO $ tryToCompile agdafile
      liftIO $ appendFile gC_log (p++(rcon))
      liftIO $ appendFile gC_log  (r++(fst answareFromGPT))
      liftIO $ cPrint "New agda file, with GTP answare \n\n" Magenta
      liftIO $ putStrLn newAfile  
      let newState = (createConvPart rcon answareFromGPT newAfile compiler (rPromptRes:rPromptReq:sPrompt)  : state)
      put newState
      case compiler of
        Nothing -> do
                   return Nothing
        Just x -> do
                  liftIO $ cPrint ("The agda compiler response with the following errors\n\n" ++ x) Red
                  -- liftIO $ rmAFile env
                  -- liftIO $ cpAFile env
                  return (Just x)



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createConvPart :: String -> (String, String) -> String -> Maybe String -> [Message] ->ConvPart
createConvPart gptIn gptOut afile acres fp =
  ConvPart{ gpt_input =gptIn
          , gpt_res =  fst gptOut
          , pure_code_res = snd gptOut
          , current_agad_file =  afile
          , agda_res = acres
          , promptL = fp
          } 

fConvInput :: AGEnv -> IO String
fConvInput env = do
    templ <- readFile $ fGptTemp env
    agda <- readFile $  (agdaFile env) 
    let x1 = replaceText templ "{function_type}" (taskDescription env)
    let x2 = replaceText x1 "{agda_code}" agda
    return x2

rConvInput :: String -> AGEnv -> String -> IO String
rConvInput cf env err = do
    templ <- readFile $ rGptTemp env
    agda <- readFile $  (agdaFile env)
    let x1 = replaceText templ "{agda_code_with_changes}" cf
    let x2 = replaceText x1 "{compiler_errors}"  err
    return x2
  
  
replaceText :: String -> String -> String -> String
replaceText [] _ _ = []
replaceText str search replace
  | L.take (L.length search) str == search = replace ++ replaceText (L.drop (L.length search) str) search replace
  | otherwise = L.head str : replaceText (L.tail str) search replace



replaceStringLoop :: String -> String -> String -> String
replaceStringLoop old new input =
    let (prefix, suffix) = breakSubstring old input
    in if suffix == "" then input
       else prefix ++ new ++ replaceStringLoop old new (drop (length old) suffix)

breakSubstring :: String -> String -> (String, String)
breakSubstring [] xs = ([], xs)
breakSubstring _ [] = ([], [])
breakSubstring str str'@(x:xs)
  | str `L.isPrefixOf` str' = ([], str')
  | otherwise = (x : prefix, suffix)
  where
    (prefix, suffix) = breakSubstring str xs


rmSubS :: String -> String -> String
rmSubS substr str = go str
  where go [] = []
        go s@(x:xs)
          | substr `L.isPrefixOf` s = L.drop (L.length substr) s
          | otherwise = x : go xs

