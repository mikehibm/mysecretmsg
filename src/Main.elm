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

keys = [
  "QWERTYUIOP",
  "ASDFGHJKL",
  "ZXCVBNM",
  " "
  ]

max_length = 90


keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey string =
  case String.uncons string of
    Just (char, "") ->
      AddLetter (String.toUpper (String.fromChar char))

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
  | EncryptClick
  | Encrypt String
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

type alias Model = {
    rawStr : String,
    encryptedStr: String,
    isKeyDialogOpen: Bool,
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
    isKeyDialogOpen = False,
    key = key,
    url = url
  }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddLetter letter ->
      ({ model |
          rawStr =
            (if (String.length model.rawStr) < max_length then
              model.rawStr ++ letter
            else
              model.rawStr
            )
      }, Cmd.none)

    Control _ ->
      (model, Cmd.none)

    Reset ->
      ({ model | rawStr = "" }, Cmd.none)

    EncryptClick ->
      ({model | isKeyDialogOpen = True }, Cmd.none)

    Encrypt key ->
      (model, Cmd.none)

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

lettersToButtons: List Char -> List (Html Msg)
lettersToButtons list =
  List.map (\x ->
    button [
        class (if x == ' ' then "space" else "key"),
        onClick (AddLetter (String.fromChar x))
      ]
      [
        text (String.fromChar x)
      ]
  ) list

renderKeyboard : Html Msg
renderKeyboard =
  div [ class "keyboard" ] (List.map (\s ->
    div [] (lettersToButtons (String.toList s))) keys)

renderKeyDialog : Model -> Html Msg
renderKeyDialog model =
  if model.isKeyDialogOpen == False then
    div [] []
  else
    div [ class "key-dialog" ] [
      text "Input a random key"
      , input [ type_ "number", value "", maxlength 4, placeholder "0000" ] []
    ]

view : Model -> Browser.Document Msg
view model =
  { title = "My Secret Message"
  , body = [
      div [ class "main" ]
        [
          div [ class "input" ]
            [
              text model.rawStr
            ]
          , div [ class "counter" ]
            [
              text (String.fromInt (max_length - String.length model.rawStr))
              , button [ class "reset", onClick Reset ] [ text "CLEAR" ]
              , button [ class "encrypt", onClick EncryptClick ] [ text "ENCRYPT / DECRYPT" ]
            ]
          , renderKeyboard
          , renderKeyDialog model
        ]
    ]
  }

