module HomePage exposing (..)
import Json.Decode exposing (Decoder, string, succeed, list)
import Json.Decode.Pipeline exposing (required)

-- DECODERS
wordDecoder : Decoder OneWord
wordDecoder = 
  succeed OneWord
    |> required "one_word" string
    |> required "descriptions" (list descriptionDecoder)

definitionsDecoder : Decoder Definitions   
definitionsDecoder = 
  succeed Definitions
    |> required "definitions" string

descriptionDecoder : Decoder Description
descriptionDecoder = 
  succeed Description
    |> required "partOfSpeech" string
    |> required "multiple_definitions" (list definitionsDecoder) 


jsonDecoder : Decoder (List OneWord)
jsonDecoder = list wordDecoder

-- MODEL
type alias Description = { partOfSpeech : String, definitions : List Definitions}

type alias Definitions = { definitions : String}
type alias OneWord = { oneWord : String, descriptions : List Description}
