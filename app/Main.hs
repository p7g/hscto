module Main where

import Control.Exception (onException)
import Control.Monad.State (StateT (StateT), evalStateT)
import Lib (deinitEditor, initEditor, runEditor)


finallyStateT :: StateT a IO b -> StateT a IO b -> StateT a IO b
finallyStateT (StateT action) (StateT cleanup) = StateT $ \s0 -> do
    (a, s1) <- action s0 `onException` cleanup s0
    (_b, s2) <- cleanup s1
    return (a, s2)


main :: IO ()
main = do
    st <- initEditor
    evalStateT (finallyStateT runEditor deinitEditor) st
