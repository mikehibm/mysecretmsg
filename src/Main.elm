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
import Encrypt exposing (encrypt)

keys = [
  "QWERTYUIOP",
  "ASDFGHJKL",
  "ZXCVBNM.?",
  " "
  ]

keysNum = [
    "123",
    "456",
    "789",
    "0"
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
  | Backspace
  | EncryptClick
  | Encrypt
  | BackToMessage
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

type InputMode = Message | EncKey

type alias Model = {
    rawStr : String,
    encKey: String,
    isKeyDialogOpen: Bool,
    mode: InputMode,
    key : Nav.Key,
    url : Url.Url
  }

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = ({
    rawStr = "",
    encKey = "",
    isKeyDialogOpen = False,
    mode = Message,
    key = key,
    url = url
  }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddLetter letter ->
      if model.mode == Message then
        ({ model |
            rawStr =
              (if (String.length model.rawStr) < max_length then
                model.rawStr ++ letter
              else
                model.rawStr
              )
        }, Cmd.none)
      else
        ({ model |
            encKey =
              (if (String.length model.encKey) < 4 then
                model.encKey ++ letter
              else
                model.encKey
              )
        }, Cmd.none)

    Control _ ->
      (model, Cmd.none)

    Reset ->
      if model.mode == Message then
        ({ model | rawStr = "" }, Cmd.none)
      else
        ({ model | encKey = "" }, Cmd.none)

    Backspace ->
      if model.mode == Message then
        ({ model | rawStr = (String.slice 0 -1 model.rawStr) }, Cmd.none)
      else
        ({ model | encKey = (String.slice 0 -1 model.encKey) }, Cmd.none)

    EncryptClick ->
      ({model | mode = EncKey }, Cmd.none)

    Encrypt ->
      ({model | mode = Message, rawStr = (encrypt model.rawStr  model.encKey) }, Cmd.none)

    BackToMessage ->
      ({model | mode = Message }, Cmd.none)

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

renderKeyboard : Model -> Html Msg
renderKeyboard model =
  div [ class "keyboard" ] (List.map (\s ->
      div [] (lettersToButtons (String.toList s)))
            (if (model.mode == Message) then keys else keysNum)
    )

renderKeyDialog : Model -> Html Msg
renderKeyDialog model =
  if model.isKeyDialogOpen == False then
    div [] []
  else
    div [ class "key-dialog" ] [
      div [ class "key-dialog-title" ] [
        div [ class "key-dialog-close" ] [
          button [] [ text "X" ]
        ]
      ]
      , text "Input a random key"
      , br [] []
      , input [
          class "key-input",
          type_ "number",
          maxlength 4,
          placeholder "0000" ] []
    ]

view : Model -> Browser.Document Msg
view model =
  { title = "My Secret Message"
  , body = [
      div [ class "main" ]
        [
          div [ class (if model.mode == Message then "input" else "input flipped") ]
            [
              text model.rawStr
            ]
          , div [ class (if model.mode == Message then "input encKey flipped" else "input encKey") ]
            [
              div [] [ text "KEY" ]
              , br [] []
              , text model.encKey
            ]
          , div [ class "counter" ]
            [
              text (String.fromInt (max_length - String.length model.rawStr))
              , button [ class "backspace", onClick Backspace ] [ text "DEL" ]
              , button [ class "reset", onClick Reset ] [ text "CLR" ]
              , br [][]
              , (if model.mode == EncKey then
                  button [ class "back", onClick BackToMessage ][ text " < "]
                else
                  text ""
                )
              , (if model.mode == Message then
                  button [ class "encrypt", onClick EncryptClick ] [ text " > " ]
                 else
                  button [ class "encrypt", onClick Encrypt ] [ text " >> " ]
              )
            ]
          , renderKeyboard model
          , renderKeyDialog model
          , div [] [text (encrypt model.rawStr model.encKey) ]
        ]
    ]
  }

