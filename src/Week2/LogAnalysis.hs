module Week2.LogAnalysis where

import Data.Char (isSpace)
import Data.List (foldl')
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

-- Ex. 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) (Node left root@(LogMessage _ rTime _) right)
    | time < rTime = Node (insert msg left) root right
    | otherwise    = Node left root (insert msg right)
-- you happy compiler?
insert _ (Node _ (Unknown _) _) = error "The tree shouldn't contain Unknown records"

-- Ex. 3
build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

-- Ex. 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

-- Ex. 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . inOrder . build . filter (isSevereError)
  where
    getMsg (LogMessage _ _ msg) = msg
    getMsg _ = "" -- for the Unknown case again
    isSevereError (LogMessage (Error sev) _ _) = sev >= 50
    isSevereError _ = False

