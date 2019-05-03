module Encrypt exposing (encrypt)

import String exposing (..)
import Char exposing (..)
import List exposing (..)
import Array exposing (..)

encrypt : String -> String -> String
encrypt rawStr encKey =
  String.toList rawStr
  |> List.indexedMap (\i ch -> (
            getIndexForEncKey i encKey
            |> getOneCharFromString '0' encKey)
            |> String.fromChar
            |> String.toInt
            |> Maybe.withDefault 0
            |> (replace ch)
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

table = [
    "VC.EAXBHIJKDL NOPQYRSU?MWTFGZ#",  -- 0
    "C.VA XHEIZDKLBNOQYRSUPMJWTF?G#",  -- 1
    "X.E A?BHIVJKDCLNOPYRSGUMWQTFZ#",  -- 2
    "ZLF?.XHURSQE ABIVJKDCNOPYMWTG#",  -- 3
    "W NAZGLF?XHURSQEBIVJ.KDCOPYMT#",  -- 4
    "YMTINWAZGL?XHUFRSQBV JE.DCOKP#",  -- 5
    "RYMIN?WAZTGLXHUCPFQBV JES.DOK#",  -- 6
    "JRY?NTGWAZ MILXKHUCES.FQBPVDO#",  -- 7
    "WA?NT JRYDOG.PFLXKHUCZEMISQBV#",  -- 8
    "TWAJRYOGC?NDFLXKH.UZSVPQE MIB#"   -- 9
  ]