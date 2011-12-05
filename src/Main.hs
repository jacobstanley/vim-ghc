module Main (main) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad
import Data.Tuple (swap)
import Data.List
import Network
import System.IO
import Text.Parsec hiding (Reply)

------------------------------------------------------------------------

main :: IO ()
main = vimServer 9191

-- | Starts a vim server on the specified port.
vimServer :: PortNumber -> IO ()
vimServer serverPort = withSocketsDo $ do
    s <- listenOn (PortNumber serverPort)

    (h, host, port) <- accept s
    putStrLn ("Received connection from " ++ host ++ ":" ++ show port)
    hSetBuffering h LineBuffering

    authenticate h
    startupLoop h

------------------------------------------------------------------------

-- | Authenticate an incoming connection to the vim server or throw
-- an error.
authenticate :: Handle -> IO ()
authenticate h = do
    msg <- hGetLine h

    let auth = "AUTH "

    unless (auth `isPrefixOf` msg)
           (detach ("Expected AUTH message, received '" ++ msg ++ "'"))

    let pwd = drop (length auth) msg

    unless (pwd == "vim-ghc")
           (detach ("Invalid password '" ++ pwd ++ "', use 'vim-ghc' \
                    \to connect to this server"))
  where
    detach msg = do
      hPutStrLn h "DETACH"
      error ("vim-ghc: " ++ msg)

------------------------------------------------------------------------

startupLoop :: Handle -> IO ()
startupLoop h = do
    msg <- receiveMsg h
    case msg of
      EventMsg 0 0 StartupDone -> mainLoop h
      _                        -> startupLoop h

mainLoop :: Handle -> IO ()
mainLoop h = forever $ do
    msg <- receiveMsg h
    case msg of
      EventMsg _ _ (FileOpened path _ _) -> putBufferNumber h path 1
      _                                  -> return ()

receiveMsg :: Handle -> IO ClientMsg
receiveMsg h = do
    str <- hGetLine h
    case parse pClientMsg "message" str of
      Left err -> error ("vim-ghc: " ++ show err)
      Right x  -> print x >> return x

putBufferNumber :: Handle -> String -> BufID -> IO ()
putBufferNumber h path bufID = sendMsg h
    (show bufID ++ ":putBufferNumber!1 " ++ vimString path)

sendMsg :: Handle -> String -> IO ()
sendMsg h msg = do
    hPutStrLn h msg
    putStrLn msg

------------------------------------------------------------------------
-- Types

type SeqNo = Int
type BufID = Int
type Name = String

data ClientMsg = ReplyMsg SeqNo Reply
               | EventMsg BufID SeqNo Event
    deriving (Show)

data Reply = UnknownReply String
    deriving (Show)

data Event = Version String
           | StartupDone
           | FileOpened FilePath Bool Bool
           | UnknownEvent Name String
    deriving (Show)

------------------------------------------------------------------------
-- Parsers

type P = Parsec String ()

pClientMsg :: P ClientMsg
pClientMsg = try pEventMsg <|> pReplyMsg

pReplyMsg :: P ClientMsg
pReplyMsg = ReplyMsg <$> pNatural <*> (optional space *> pReply)

pReply :: P Reply
pReply = UnknownReply <$> pRemaining

pEventMsg :: P ClientMsg
pEventMsg = do
    bufID <- pNatural
    char ':'
    name <- many1 letter
    char '='
    seqNo <- pNatural
    optional space
    event <- pEvent name
    return (EventMsg bufID seqNo event)

pEvent :: Name -> P Event
pEvent name = case name of
    "version"     -> Version <$> pVimString
    "startupDone" -> return StartupDone
    "fileOpened"  -> FileOpened <$> pVimString <*> pVimBool <*> pVimBool
    _             -> UnknownEvent name <$> pRemaining

pNatural :: P Int
pNatural = read <$> many1 digit

pRemaining :: P String
pRemaining = manyTill anyChar eof

-- | Runs a parser then expects a space or the end of
-- the input stream.
pArg :: P a -> P a
pArg p = p <* (space *> return () <|> eof)

pVimBool :: P Bool
pVimBool = pArg $
    char 'T' *> return True <|>
    char 'F' *> return False

pVimString :: P String
pVimString = pArg $
    char '"' *> many (escaped <|> noneOf "\"") <* char '"'
  where
    escaped = char '\\' >> choice (map escapedChar vimStringEscapes)
    escapedChar (code, replacement) = char code >> return replacement

------------------------------------------------------------------------
-- Vim Strings

vimString :: String -> String
vimString xs = "\"" ++ escape xs ++ "\""
  where
    escape  = concatMap escapeChar
    escapes = map swap vimStringEscapes

    escapeChar c = case lookup c escapes of
      Just ec -> ['\\', ec]
      Nothing -> [c]

vimStringEscapes :: [(Char, Char)]
vimStringEscapes =
    [ ('n',  '\n')
    , ('r',  '\r')
    , ('t',  '\t')
    , ('\"', '\"')
    , ('\\', '\\') ]
