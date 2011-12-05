module Main (main) where

import Control.Monad
import Data.List
import Network
import System.IO

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
    s <- listenOn (PortNumber 9191)

    (h, host, port) <- accept s
    putStrLn ("Received connection from " ++ host ++ ":" ++ show port)

    authenticate h

    forever (hGetLine h >>= putStrLn)

    return ()

authenticate :: Handle -> IO ()
authenticate h = do
    msg <- hGetLine h

    unless (auth `isPrefixOf` msg)
           (detach ("Expected AUTH message, received '" ++ msg ++ "'"))

    let pwd = drop (length auth) msg

    unless (pwd == "vim-ghc")
           (detach ("Invalid password '" ++ pwd ++ "', use 'vim-ghc' \
                    \to connect to this server"))
  where
    auth = "AUTH "

    detach msg = do
      hPutStrLn h "DETACH"
      error ("vim-ghc: " ++ msg)
