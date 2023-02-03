module Main exposing(..)
import String
import Browser
import Html exposing (..)
import Http
import Html.Events exposing (..)
import Array exposing (Array)
import Array exposing (..)
import HomePage exposing (..)
import GrabMaybe exposing(..)
import Array exposing (empty)
import Random exposing(..)
import Html.Attributes exposing(..)

-- MAIN

main : Program () Model Msg
main = 
  Browser.element
    { 
      init = init
      , update = update
      , subscriptions = subscriptions
      , view = view}
path_to_words : String
path_to_words = "words_and_design/words.txt"
link_to_api : String
link_to_api = "https://api.dictionaryapi.dev/api/v2/entries/en/hello"


-- MODEL

type alias Model = { descriptions : List Description , victory : Bool, loading: Bool, error: String, guess_ : String , tobeGuessed : String, wordsList: Array String , showGuess: Bool, displayGuess : Bool} 

empty_model : Model   --is a value of type Model with all fields set to default values.
empty_model= Model [] False False "" "" "" empty  False False

-- INIT
init : flags -> (Model, Cmd Msg)
init _ = ({empty_model | loading = True}, getting_words)





-- UPDATE
type Msg = Convert String | Print Bool | Json (Result Http.Error (List OneWord)) | GenWord Int  | HaveWords (Result Http.Error String)
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Convert guess_ ->
      if guess_ == model.tobeGuessed then ({model | victory=True}, Cmd.none) ---If the message is Convert, the function checks if the received guess is equal to the word to be guessed. If it is, then the victory field in the model is set to True.
      else ({model | guess_=guess_}, Cmd.none)
    Print print -> ({model | displayGuess=print}, Cmd.none)  ---If the message is Print, the function updates the displayGuess field in the model.
    HaveWords result -> ----If the message is HaveWords, the function checks if the result is Ok, meaning the words from the file were successfully loaded. If the result is Ok, the words are extracted from the result and are converted into an Array of strings.
      case result of
      Ok text -> ({model | wordsList = (Array.fromList (String.split " " text))}, random_function (Array.fromList (String.split " " text)))
      Err _ -> ({model | error = "Can't load your words", loading = False}, Cmd.none)
    GenWord newNum -> let anotherWord = takeString (Array.get (newNum) model.wordsList) in
      ({model | tobeGuessed = anotherWord}, getting_descriptions anotherWord)
    Json result ->     ---If the message is Json, the function checks if the result is Ok, meaning the API request for the word descriptions was successful. If the result is Ok, the function extracts the descriptions from the JSON response and updates the descriptions field in the model.
      case result of
      Ok json -> ({model | descriptions = (takeWord (List.head json)).descriptions, loading = False }, Cmd.none)
      Err _ -> ({model | error = "Can't load API", loading = False}, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW
--functions-- to do

view : Model -> Html Msg
view model =
  div [id "container"] [
    if model.loading then
      h1 [][text " Loading ..."]
    else if model.error /= "" then
      h2 [class "highlight"][text "Error: " ]
    else if model.victory then
      h1 [][text "That is right!"]
    else
      div [class "top"] [
        h1 [][text "Make a guess"],
        if model.displayGuess then
          h2 [][text "The answer was "]
        else
          text "",
        descriptions_html model.descriptions,
        div [class "top"] []
      ]
  ]
getting_words : Cmd Msg   --is a command that sends an HTTP GET request to the words.txt file.
getting_words = 
  Http.get
    { url = "words_and_design/words.txt"
    , expect = Http.expectString HaveWords
    }

getting_descriptions : String -> Cmd Msg   --is a function that takes a word as an argument and sends an HTTP GET request to the API to get the description of the word.
getting_descriptions get_word = 
  Http.get
    { url = link_to_api++get_word
    , expect = Http.expectJson Json jsonDecoder
    }
 


random_function: Array String -> Cmd Msg
random_function array_ = Random.generate GenWord (Random.int 0 ((Array.length array_)))  --is a function that takes a list of descriptions and returns the HTML to display them.


descriptions_html : List Description -> Html Msg
descriptions_html list = ul [] (List.map (\description -> li [] [h2[][text description.partOfSpeech], def_function description.definitions]) list)  --  is a function that takes a list of descriptions and returns the HTML to display them.
def_function : List Definitions -> Html Msg
def_function list = ol [] (List.map (\def -> li [] [text def.definitions]) list)  -- is a function that takes a list of definitions and returns the HTML to display them.