module Week2.LogAnalysis where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe, listToMaybe)

import Week2.Log

-- Ex. 1
parseMessage :: String -> LogMessage
parseMessage line = case line of
    'I':rest -> fromMaybe unknown $ maybeReadInfo rest
    'W':rest -> fromMaybe unknown $ maybeReadWarn rest
    'E':rest -> fromMaybe unknown $ maybeReadErr  rest
    _        -> unknown
  where
    unknown = Unknown line
    maybeReadInfo = maybeReadTime Info
    maybeReadWarn = maybeReadTime Warning

    maybeReadErr :: String -> Maybe LogMessage
    maybeReadErr str = do
        (sev, rest) <- listToMaybe $ reads str
        maybeReadTime (Error sev) rest

    maybeReadTime :: MessageType -> String -> Maybe LogMessage
    maybeReadTime level str = do
        (time, msg) <- listToMaybe $ reads str
        return $ LogMessage level time $ dropWhile isSpace msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

