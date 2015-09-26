{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (parse,whatWentWrong) where
import Log

--exercise 1
parseNumber :: String -> Maybe Int
parseNumber n = case reads n of
                     [(x,"")] -> Just x
                     _ -> Nothing            

parseLog :: [String] -> Maybe (MessageType,[String])
parseLog [] = Nothing
parseLog ("I" : rest) = Just (Info, rest)
parseLog ("W" : rest) = Just (Warning,rest)
parseLog ("E" : n : rest) = case parseNumber n of
                                 Nothing -> Nothing
                                 Just num -> Just (Error num, rest)
parseLog _ = Nothing

parseMessage :: String -> LogMessage
parseMessage  = parseMessage' . words 

parseMessage' :: [String] -> LogMessage
parseMessage' []  =  Unknown ""
parseMessage' strings = case parseLog strings of
  Just (messageType,(x:xs)) -> case parseNumber x of
      Nothing -> Unknown . unwords $ strings
      Just timeStamp -> LogMessage messageType timeStamp $ unwords xs
  _ -> Unknown . unwords $ strings  

parse :: String -> [LogMessage]
parse = map parseMessage . lines


--exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert m@(LogMessage _ time _) (Node left node@(LogMessage _ ctime  _) right)
  |time > ctime = Node left node (insert m right)
  |time < ctime = Node (insert m left) node right
insert _ _ = error "insert assumes that timestamps are unique and that MessageTrees are sorted and don't contain Unknowns"

--exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

--exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++ [node] ++ inOrder right

-- exercise 5

printMessage :: LogMessage -> String
printMessage (LogMessage _ _ s) = s
printMessage (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map printMessage . filter relevant . inOrder . build
  where relevant :: LogMessage -> Bool
        relevant (LogMessage (Error severity) _ _ ) = severity >= 50
        relevant _ = False



