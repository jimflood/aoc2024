module Lib
    ( slurpLines
    ) where

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename
