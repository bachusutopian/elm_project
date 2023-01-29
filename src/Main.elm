module Main exposing(..)
import String
import Browser
import Html exposing (Html, text, pre)
import Http
import Html.Events exposing (..)




-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Model
  = Failure
  | Loading
  | Success String

--type alias List_Of_Words =
  
 ---     List_words : List String



init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "http://localhost:8000/words.txt"
      , expect = Http.expectString GotText
      }
    

  )




-- UPDATE


type Msg
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)
          
 

-- GUESS



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      let 
        listOfWords = String.split "  ," fullText
      in
      pre [] (List.map (\word -> text word) listOfWords)

