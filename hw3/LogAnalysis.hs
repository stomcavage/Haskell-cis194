{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.ParserCombinators.Parsec 

-- Exercise 1: Parse a line in the log file
parseLogInfo :: Parser MessageType
parseLogInfo = char 'I' >> return Info

parseLogWarn :: Parser MessageType
parseLogWarn = char 'W' >> return Warning

parseLogErr :: Parser MessageType
parseLogErr = do _ <- char 'E' 
                 _ <- spaces
                 x <- many1 digit
                 return $ Error (read x)

parseLogType :: Parser MessageType
parseLogType = try parseLogInfo <|> try parseLogWarn <|> parseLogErr

parseTimeStamp :: Parser TimeStamp
parseTimeStamp = do x <- many1 digit
                    return $ read x

parseLogLine :: Parser LogMessage
parseLogLine = do logType <- parseLogType 
                  spaces
                  logTime <- parseTimeStamp 
                  spaces
                  logMsg  <- many1 anyChar
                  return (LogMessage logType logTime logMsg)

parseMessage :: String -> MaybeLogMessage
parseMessage message = case parse parseLogLine "log" message of
                            Left _    -> InvalidLM message 
                            Right val -> ValidLM val

{-
-- Exercise 2: Discard invalid log lines
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly maybeLogMessages = filter (\m -> (LogMessage m)) maybeLogMessages

-- Exercise 3: Parse an entire log file
parse :: String -> [LogMessage]
parse logFile = validMessagesOnly [parseMessage message | message <- lines logFile]

-- Exercise 4: Provide a comparison function for use in sorting
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ a _) (LogMessage _ b _) = a `compare` b

-- Exercise 5: Provide a sorting function
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages a b = sort compareMsgs a b
-}

