module Encrypt exposing (encrypt)

import String exposing (..)
import Char exposing (..)
import List exposing (..)
import Array exposing (..)

encrypt : String -> String -> String
encrypt rawStr encKey =
  String.toList rawStr
  |> List.indexedMap (\i ch ->
            getIndexForEncKey i encKey
            |> getOneCharFromString '0' encKey
            |> String.fromChar
            |> String.toInt
            |> Maybe.withDefault 0
            |> (+) i
            |> modBy (List.length table)
            |> replace ch
          )
  |> String.fromList

getIndexForEncKey : Int -> String -> Int
getIndexForEncKey i encKey =
  let
    len = String.length encKey
  in
    if len > 0 then
      modBy len i
    else
      0

getOneCharFromString : Char -> String -> Int -> Char
getOneCharFromString defaultChar str index =
  if index >= 0 then
    String.slice index (index + 1) str
    |> String.toList
    |> head
    |> Maybe.withDefault defaultChar
  else
    defaultChar

getFromTable : Int -> String
getFromTable n =
  get n (Array.fromList table)
  |> Maybe.withDefault ""

findIndex : Char -> String -> Int
findIndex ch tableStr =
  let
    ix = tableStr
      |> String.indexes (String.fromChar ch)
      |> List.head
  in
    case ix of
      Just i ->
        if (modBy 2 i) == 0 then i + 1 else i - 1
      Nothing -> -1

replace : Char -> Int -> Char
replace ch n =
  let
    tableStr = getFromTable n
  in
    findIndex ch tableStr
    |> getOneCharFromString '_' tableStr

-- ランダムな置換テーブル。
-- 奇数位置の文字と偶数位置の文字が置換ペア。
-- 長さが奇数の場合は必ず末尾に'#'を付けて偶数になるようにする。
table = [
    "VC.EAXBHIJKDL NOPQYRSU?MWTFGZZ",  -- 0
    "C.VA XHEIZDKLBNOQYRSUPMJWTF?GG",  -- 1
    "X.E A?BIHVJKDCLNOPYRSGUMWQTFZZ",  -- 2
    "ZLF?.XHURSQE ABIVJKDCNOPYMWGTT",  -- 3
    "W NAZGLF?XHURSQEBIVJ.KDCOPYTMM",  -- 4
    "YMTINWAZGL?XHUFRSQBV JE.DCOKPP",  -- 5
    "RYMINA?WZTLGXHUCPFQBV JES.DOKK",  -- 6
    "JRY?NTGWAZ MILXKHUCES.FQBPVDOO",  -- 7
    "WA?NT JRYDOG.PFLXKHUCZEMISQBVV",  -- 8
    "TWAJRYOGC?NDFLKXH.UZSVPQE MIBB"   -- 9
  ]