{-# LANGUAGE OverloadedStrings #-}
module Reki where

import qualified Data.Text as T
import File (fileRead)

data KWord = Hito T.Text | Mono T.Text | Atum T.Text | Koto T.Text deriving (Eq,Show)

type Nen = Int
type Disc = T.Text
data Grade = A | B | C deriving (Eq,Show)

data HData = HData Grade Nen Disc [KWord] deriving (Eq,Show)

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
      (n:d:ks) = pdt
      (gr,nen) = makeNen n 
      kws = map makeKW ks
   in HData gr nen d kws

makeNen :: T.Text -> (Grade,Nen)   
makeNen txt
  | txt==T.empty = (C,0)
  | T.head txt=='!' = (B,(read . T.unpack . T.tail) txt)
  | otherwise = (A,(read. T.unpack) txt)

makeKW :: T.Text -> KWord
makeKW k = 
  if k==T.empty then Koto " " else let tk = T.tail k in
          case T.head k of
             'h' -> Hito tk 
             'm' -> Mono tk 
             'k' -> Koto tk 
             'a' -> Atum tk 
             _   -> Koto k

fillList :: [T.Text] -> [T.Text]
fillList ls
  | length ls > 3 = ls
  | otherwise = fillList (ls<>[" "])

