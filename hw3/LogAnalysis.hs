{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Char
import Data.List
import Log
import Text.ParserCombinators.Parsec as P

-- Exercise 1: Parse a line in the log file
parseLogInfo :: Parser MessageType
parseLogInfo = P.char 'I' >> return Info

parseLogWarn :: Parser MessageType
parseLogWarn = P.char 'W' >> return Warning

parseLogErr :: Parser MessageType
parseLogErr = do _ <- P.char 'E' 
                 _ <- P.spaces
                 x <- P.many1 P.digit
                 return $ Error (read x)

parseLogType :: Parser MessageType
parseLogType = try parseLogInfo <|> try parseLogWarn <|> parseLogErr

parseTimeStamp :: Parser TimeStamp
parseTimeStamp = do x <- P.many1 P.digit
                    return $ read x

parseLogLine :: Parser LogMessage
parseLogLine = do logType <- parseLogType 
                  P.spaces
                  logTime <- parseTimeStamp 
                  P.spaces
                  logMsg  <- P.many1 P.anyChar
                  return (LogMessage logType logTime logMsg)

parseMessage :: String -> MaybeLogMessage
parseMessage message = case P.parse parseLogLine "log" message of
                            Left _    -> InvalidLM message 
                            Right val -> ValidLM val

-- Exercise 2: Discard invalid log lines
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly ms = [m | ValidLM m <- ms]

-- Exercise 3: Parse an entire log file
parseLog :: String -> [LogMessage]
parseLog logFile = validMessagesOnly [parseMessage m | m <- lines logFile]

-- Exercise 4: Provide a comparison function for use in sorting
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ a _) (LogMessage _ b _) = a `compare` b

-- Exercise 5: Provide a sorting function
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages ms = sortBy compareMsgs ms

-- Exercise 6: Extract error messages with severity of 50 or greater
errorFilter :: LogMessage -> Bool
errorFilter (LogMessage (Error e) _ _) | e >= 50 = True
errorFilter _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map (\(LogMessage _ _ m) -> m) (sortMessages $ filter errorFilter ms)

-- Exercise 7: Case-insensitive search for a string in a list of messages
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout search ms = filter (\(LogMessage _ _ m) -> isInfixOf (map toLower search) (map toLower m)) ms

