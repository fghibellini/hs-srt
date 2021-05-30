{-# LANGUAGE BlockArguments #-}

import Criterion.Main
import Lib (parse, SubRip, RawLine, contents, unRawLine)
import Lib as Lib

example1k, example10k, example100k :: String
example1k =  "./data/s01e03-1k.srt"
example10k =  "./data/s01e04-10k.srt"
example100k =  "./data/s01e05-100k.srt"

benchParseFile :: String -> Benchmark
benchParseFile f = bench ("benchParseFile " <> f) $ whnfIO do
  subs <- ((parse f) :: IO (SubRip RawLine))
  pure subs
  
main = defaultMain [
  bgroup "File parsing"
    [ benchParseFile example1k
    , benchParseFile example10k
    , benchParseFile example100k
    ]
  ]
