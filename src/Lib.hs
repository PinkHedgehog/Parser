module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ sum [1..3]
