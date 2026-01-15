module MyLib (someFunc) where

import Journal (run)

someFunc :: IO ()
someFunc = do
    -- putStrLn "someFunc"/
    run
