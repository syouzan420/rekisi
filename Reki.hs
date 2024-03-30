{-# LANGUAGE OverloadedStrings #-}
module Reki where

import qualified Data.Text as T
import File (fileRead)

data KWord = Hito T.Text | Mono T.Text | Koto T.Text deriving (Eq,Show)

type Nen = Int
type Disc = T.Text

data HData = HData Nen KWord Disc [KWord] deriving (Eq,Show)

rekifile :: FilePath
rekifile = "reki.txt"

main :: IO ()
main = do
  rd <- fileRead rekifile
  let rdls = T.lines rd
      hdata = map makeHData rdls
  mapM_ (putStrLn . T.unpack) rdls 
  mapM_ print hdata

makeHData :: T.Text -> HData
makeHData txt =
  let sps = T.splitOn "," txt
      pdt = if length sps > 3 then sps else fillList sps
      (n:k:d:ks) = pdt
      nen = read$T.unpack n
      kw = makeKW k
      kws = map makeKW ks
   in HData nen kw d kws

makeKW :: T.Text -> KWord
makeKW k = case T.head k of
             'h' -> Hito (T.tail k)
             'm' -> Mono (T.tail k)
             'k' -> Koto (T.tail k)
             _   -> Koto k

fillList :: [T.Text] -> [T.Text]
fillList ls
  | length ls > 3 = ls
  | otherwise = fillList (ls<>[" "])

