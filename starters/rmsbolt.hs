-- Haskell rmsbolt starter file

-- Haskell demangler support can be gained by placing the binary
-- generated from this folder on your path under the name
-- 'haskell-demangler'
-- https://github.com/mattgodbolt/compiler-explorer/tree/master/haskell

-- Local Variables:
-- rmsbolt-command: "ghc -O0"
-- End:

module Rmsbolt where

import Data.Char

isRMS :: Char -> Bool
isRMS letter
  | letter == 'R' = True
  | letter == 'M' = True
  | letter == 'S' = True
  | otherwise = False

main :: IO()
main =
  let num = Data.Char.chr 2 in
    let out = isRMS num in
      let out_str =
            if out then
              "True" else
              "False" in
        putStrLn out_str
