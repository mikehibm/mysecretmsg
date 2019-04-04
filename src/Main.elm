module Main exposing (main)

import String
import Browser
import Browser.Navigation as Nav
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url
import Json.Decode as Decode

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letters = ["A", "B", "C"]

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey string =
  case String.uncons string of
    Just (char, "") ->
      AddLetter (String.fromChar char)

    _ ->
      Control string

main =
  Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


type Msg
  = AddLetter String
  | Control String
  | Reset
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

type alias Model = {
    rawStr : String,
    encryptedStr: String,
    key : Nav.Key,
    url : Url.Url
  }

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = ({
    rawStr = "",
    encryptedStr = "",
    key = key,
    url = url
  }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddLetter letter ->
      ({ model | rawStr = model.rawStr ++ letter }, Cmd.none)

    Control _ ->
      (model, Cmd.none)

    Reset ->
      ({ model | rawStr = "" }, Cmd.none)

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

alphabetToLetters: String -> List Char
alphabetToLetters str = String.toList alphabet

lettersToButtons: List Char -> List (Html Msg)
lettersToButtons list =
  List.map (\x ->
    button [onClick (AddLetter (String.fromChar x))]
    [ text (String.fromChar x)]) list


view : Model -> Browser.Document Msg
view model =
  { title = "My Secret Message"
  , body = [
      div [ class "main" ] [
        div [] [ text model.rawStr, text " / ",  text (String.fromInt (String.length model.rawStr)) ]
        , button [ class "reset", onClick Reset ] [ text "Reset" ]
        , br [] []
        , div [] (lettersToButtons (alphabetToLetters alphabet))
      ]
    ]
  }

