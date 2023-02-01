module GrabMaybe exposing (..)
import HomePage exposing (OneWord)

takeWord : Maybe OneWord -> OneWord
takeWord first = 
  case first of 
    Just one_word -> one_word
    Nothing -> OneWord "" [] 

takeString : Maybe String -> String
takeString first = 
  case first of 
    Just str -> str
    Nothing -> ""

