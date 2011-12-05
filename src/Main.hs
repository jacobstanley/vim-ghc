module Main (main) where

import Control.Monad
import System.Directory (canonicalizePath)
import System.FilePath (dropFileName)
import Language.Haskell.Interpreter

main :: IO ()
main = do
    --path <- canonicalizePath "../ghc-mod/GHCMod.hs"
    path <- canonicalizePath "examples/Test.hs"
    --putStr ("Loading " ++ path ++ "...")

    xs <- runInterpreter $ replicateM 100 $ do
        set [searchPath := [dropFileName path]]

        loadModules [path]
        modules <- getLoadedModules
        setTopLevelModules modules
        --liftIO (putStrLn "done")

        t <- typeOf "add"
        liftIO (putStrLn ("add :: " ++ t))

        e1 <- interpret "pwd" infer
        liftIO (e1 >>= putStrLn)

        e2 <- eval "add 1 (add 4 5)"
        liftIO (putStrLn e2)

        exp <- getModuleExports "System.Directory"
        liftIO (print exp)

    case xs of
        Left x -> print x
        _      -> return ()
