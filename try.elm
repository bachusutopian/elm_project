module Main exposing (..)

import Http
import Json.Decode as Decode
import Random

-- Types to represent the state of the game
type alias Model =
    { word : String
    , definitions : List String
    , guess : String
    , isCorrect : Bool
    }

-- Type for possible messages
type Msg
    = GetWord
    | GotWord (Result Http.Error (String, List String))
    | UpdateGuess String
    | CheckGuess
    | Correct
    | Incorrect

-- Function to initialize the state of the game
init : (Model, Cmd Msg)
init =
    ( { word = ""
      , definitions = []
      , guess = ""
      , isCorrect = False
      }
    , Cmd.none
    )

-- Function to update the state based on messages
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetWord ->
            ( model, getWord )

        GotWord (Ok (word, definitions)) ->
            ( { model | word = word, definitions = definitions }, Cmd.none )

        GotWord (Err _) ->
            ( model, Cmd.none )

        UpdateGuess guess ->
            ( { model | guess = guess }, Cmd.none )

        CheckGuess ->
            ( model, checkGuess model.guess )

        Correct ->
            ( { model | isCorrect = True }, Cmd.none )

        Incorrect ->
            ( { model | guess = "", isCorrect = False }, Cmd.none )

-- Helper function to get a random word and its definitions from the online file
getWord : Cmd Msg
getWord =
    Http.send GotWord <|
        Http.get
            { url = "https://your-server.com/path-to-word-list.txt"
            , expect = Http.expectString
            }
            |> Random.map (Random.int 0)
            |> Cmd.map (\randomIndex ->
                ( getWordAtIndex randomIndex
                , getDefinitionsForWord (getWordAtIndex randomIndex)
                )
            )

-- Helper function to get the word at a specific index from the online file
--getWordAtIndex : Int -> String
--getWordAtIndex index =
    -- Code to get the word at the specified index from the online file

-- Helper function to get the definitions for a specific word from the dictionary API
--getDefinitionsForWord : String -> List String
getDefinitionsForWord word =
    Http.send GotDefinitions <|
        Http.get
            { url = "https://www.dictionaryapi.com/api/v3/references/collegiate/json/" ++ word
            , expect = Http.expectJson (Decode.list (Decode.field "shortdef" Decode.string))
            } 


-- Helper function to check if the user's guess is correct
checkGuess : String -> Cmd Msg
checkGuess guess =
    if guess == model.word then
    Cmd.msg Correct
    else
    Cmd.msg Incorrect

-- View function to display the current state of the game
view : Model -> Html Msg
view model =
    div []
    [ h1 [] [text model.word]
    , h2 [] [text "Definitions:"]
    , ul [] (List.map (li [] << text) model.definitions)
    , input [ onInput UpdateGuess, value model.guess ] []
    , button [ onClick CheckGuess ] [text "Guess"]
    , if model.isCorrect then
    h3 [] [text "Correct!"]
    else if model.guess /= "" then
    h3 [] [text "Incorrect."]
    else
    text ""
    ]

-- Subscription function to handle any additional effects
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- Main function to run the game
main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

