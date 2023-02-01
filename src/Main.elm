module Main exposing(..)
import String
import Browser
import Html exposing (..)
import Html exposing (Html, text, pre)
import Http
import Html.Events exposing (..)
import Random
import Array exposing (Array)
import HomePage exposing (..)
import GrabMaybe exposing(..)
import HomePage exposing (OneWord)
import Array exposing (empty)

-- MAIN

main =
  Browser.element
    { init = init, update = update, subscriptions = subscriptions, view = view
    }

api : String
api = "https://api.dictionaryapi.dev/api/v2/entries/en/"

path_to_words : String
path_to_words = "words_and_design/words.txt"


-- MODEL

type alias Model = { descriptions : List Description , victory : Bool, loading: Bool, error: String, guess_ : String 
, tobeGuessed : String, wordsList: Array String , showGuess: Bool, displayGuess : Bool
  } 

emptyModel : Model
emptyModel = Model [] False False "" "" "" empty  False False

-- INIT
init : flags -> (Model, Cmd Msg)
init _ = 

  ({emptyModel | loading = True}, getting_words)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none




-- UPDATE
type Msg = Convert String | Print Bool | GotJson (Result Http.Error (List OneWord)) | GenWord Int  | HaveWords (Result Http.Error String)
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Convert guess_ ->
      if guess_ == model.tobeGuessed then ({model | victory=True}, Cmd.none)
      else ({model | guess_=guess_}, Cmd.none)
    Print print -> ({model | displayGuess=print}, Cmd.none)
    HaveWords result ->
      case result of
      Ok text -> ({model | wordsList = (Array.fromList (String.split " " text))}, getRandomInt (Array.fromList (String.split " " text)))
      Err _ -> ({model | error = "Can't load your words", loading = False}, Cmd.none)
    GenWord newInt -> let anotherWord = takeString (Array.get (newInt) model.wordsList) in
      ({model | tobeGuessed = anotherWord}, getting_descriptions anotherWord)
    GotJson result ->
      case result of
      Ok json -> ({model | descriptions = (takeWord (List.head json)).descriptions, loading = False }, Cmd.none)
      Err _ -> ({model | error = "Can't load API", loading = False}, Cmd.none)

-- VIEW
--functions-- to do
getting_words : Cmd Msg
getting_words = 
  Http.get
    { url = path_to_words
    , expect = Http.expectString HaveWords
    }

getting_descriptions : String -> Cmd Msg
getting_descriptions get_word = 
  Http.get
    { url = api++get_word
    , expect = Http.expectJson GotJson jsonDecoder
    }
 






view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      let 
        listOfWords = String.split "  " fullText
      in
        pre [] (List.map (\word -> text word) listOfWords)
