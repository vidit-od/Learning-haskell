{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Log where

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO ()
testParse parse n file = do
    content <- readFile file
    let msgs = take n $ parse content
    mapM_ print msgs
-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

parseMessage :: String -> LogMessage
parseMessage str =
  case words str of
    ("I" : ts : msg) -> LogMessage Info (read ts) (unwords msg)
    ("W" : ts : msg) -> LogMessage Warning (read ts) (unwords msg)
    ("E" : code : ts : msg) -> LogMessage (Error (read code)) (read ts) (unwords msg)
    s -> Unknown (unwords s)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


getTime :: LogMessage -> Maybe TimeStamp
getTime (LogMessage _ ts _) = Just ts
getTime (Unknown _) = Nothing

-- hakell is immutalble, so each insert we create a new tree
insertLog :: LogMessage -> MessageTree -> MessageTree
insertLog msg Leaf = Node Leaf msg Leaf
insertLog msg (Node left nodemsg right) =
  case (getTime msg , getTime nodemsg) of
     (Nothing, _) -> Node left nodemsg right
     (_, Nothing) -> Node left nodemsg right
     (Just ts1, Just ts2) ->
        if ts1 < ts2
          then Node (insertLog msg left) nodemsg right
          else Node left nodemsg (insertLog msg right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insertLog x (build xs)

inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left nodeMsg right) = 
  inorder left ++ [nodeMsg] ++ inorder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong[] = []
whatWentWrong (x:xs) = 
  case x of 
    LogMessage (Error sevirity) _ msg | sevirity >= 50 -> msg : whatWentWrong xs
    _ -> whatWentWrong xs
  
main = do
  myFile <- readFile "g:\\coding\\github\\Haskell\\week-2\\sample.log"
  let parsedFile = parse myFile
  let tree = build parsedFile
  print( whatWentWrong parsedFile)
  print " Done "