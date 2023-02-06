module Main exposing(..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as D exposing (Decoder)
import Random

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
type alias Meaning =
    { partOfSpeech : String
    , defs : List String
    }


type Status
    = Loading
    | Error String
    | Guessing
    | Victory


type alias Model =
    { wordList : List String
    , meanings : List Meaning
    , toBeGuessed : String
    , guess : String
    , status : Status
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { wordList = []
    , meanings = []
    , toBeGuessed = ""
    , guess = ""
    , status = Loading
    }
    , Http.get
        { url = "words_and_design/words.txt"
        , expect = Http.expectString GotWordbook
        }
    )


view : Model -> Html Msg

view model =
    let
        viewDef def =
            li [] [ text def ]

        viewMeaning meaning =
            ul []
                [ li []
                    [ text meaning.partOfSpeech
                    , ol [] (List.map viewDef meaning.defs)
                    ]
                ]
        guessingView =
            div []
                [ h1 [] [ text "Guess the word!" ]
                , p [] [ text "Meanings:" ]
                , ul [] (List.map viewMeaning model.meanings)
                , p [] [ text "Enter your guess: " ]
                , input [ onInput ChangedGuess ] []
                , button [ onClick (ChangedGuess model.guess) ] [ text "Submit" ]
                ]
    in
    case model.status of
        Loading ->
            div [] []

        Error errorMsg ->
            h1 [] [ text ("Error: " ++ errorMsg) ]

        Guessing ->
            guessingView

        Victory ->
            h1 [] [ text "You win!" ]







getDescriptions : String -> Cmd Msg
getDescriptions word =
    let
        apiUrl =
            "https://api.dictionaryapi.dev/api/v2/entries/en/"
    in
    Http.get
        { url = apiUrl ++ word
        , expect = Http.expectJson GotJson jsonDecoder
        }


getRandomWord : String -> List String -> Cmd Msg
getRandomWord firstWord restWords =
    Random.generate GotRandomWord (Random.uniform firstWord restWords)


type Msg
    = GotWordbook (Result Http.Error String)
    | GotRandomWord String
    | GotJson (Result Http.Error (List Meaning))
    | ChangedGuess String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWordbook result ->
            case result of
                Ok wordbook ->
                    let
                        wordList =
                            String.words wordbook
                    in
                    case wordList of
                        [] ->
                            ( { model | status = Error "Empty wordbook" }
                            , Cmd.none
                            )

                        firstWord :: restWords ->
                            ( { model | wordList = wordList }
                            , getRandomWord firstWord restWords
                            )

                Err _ ->
                    ( { model | status = Error "Can't load your words" }, Cmd.none )

        GotRandomWord word ->
            ( { model | toBeGuessed = word }
            , getDescriptions word
            )

        GotJson result ->
            case result of
                Ok meanings ->
                    ( { model
                        | meanings = meanings
                        , status = Guessing
                    }
                    , Cmd.none
                    )

                Err (BadBody json) ->
                    ( { model
                        | status = Error ("Json parsing failed: \n" ++ json)
                    }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | status = Error "Http error" }, Cmd.none )

        ChangedGuess guess ->
            if guess == model.toBeGuessed then
                ( { model
                    | guess = guess
                    , status = Victory
                }
                , Cmd.none
                )

            else
                ( { model | guess = guess }, Cmd.none )


-- DECODERS


defDecoder : Decoder String
defDecoder =
    D.field "definition" D.string


meaningDecoder : Decoder Meaning
meaningDecoder =
    D.map2 Meaning
        (D.field "partOfSpeech" D.string)
        (D.field "definitions" (D.list defDecoder))


jsonDecoder : Decoder (List Meaning)
jsonDecoder =
    -- the "0" in D.at takes advantage of the fact that
    -- arrays in js are actually objects, with the keys being an array index
    D.at [ "0", "meanings" ] (D.list meaningDecoder)