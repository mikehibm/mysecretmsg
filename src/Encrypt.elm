module Encrypt exposing (..)

import String exposing (..)
import Char exposing (..)
import Basics exposing (..)
import Bitwise exposing (..)
import Array

encrypt : String -> String -> String
encrypt rawStr encKey =
  map (\ch -> (replace ch 1)) rawStr

getFromTable : Int -> String
getFromTable n =
  case (Array.get n (Array.fromList table)) of
    Just str -> str
    Nothing -> ""

findIndex : Char -> Int -> Int
findIndex ch n =
  case (Array.get 0 (Array.fromList (String.indexes (String.fromChar ch) (getFromTable n)))) of
    Just ix -> ix
    Nothing -> 0

getNewIndex : Int -> Int
getNewIndex i =
  if (modBy 2 i) == 0 then
    i + 1
  else
    i - 1

replace : Char -> Int -> Char
replace ch n =
  case (List.head (String.toList (String.slice ((findIndex ch n) |> getNewIndex) (((findIndex ch n) |> getNewIndex) + 1) (getFromTable n)))) of
    Just c -> c
    Nothing -> ' '

table = [
    -- "ABCDEFGHIJKLMNOPQRSTUVWXYZ !?",
    "VC!EAXBHIJKDL NOPQYRSU?MWTFGZ",
    "C!VA XHEIZDKLBNOQYRSUPMJWTF?G",
    "X!E A?BHIVJKDCLNOPYRSGUMWQTFZ",
    "ZLF?!XHURSQE ABIVJKDCNOPYMWTG",
    "W NAZGLF?XHURSQEBIVJ!KDCOPYMT",
    "YMTINWAZGL?XHUFRSQBV JE!DCOKP",
    "RYMIN?WAZTGLXHUCPFQBV JES!DOK",
    "JRY?NTGWAZ MILXKHUCES!FQBPVDO",
    "WA?NT JRYDOG!PFLXKHUCZEMISQBV",
    "TWAJRYOGC?NDFLXKH!UZSVPQE MIB"
  ]