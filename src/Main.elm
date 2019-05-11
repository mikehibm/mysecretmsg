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
max_length_key = 4


keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey string =
  case String.uncons string of
    Just (char, "") ->
      AddLetter (String.toUpper (String.fromChar char))
    _ ->
      case string of
        "Backspace" -> BackspaceKey
        "Enter" -> EnterKey
        _ -> Control string

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
  | BackspaceKey
  | EnterKey
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

type InputMode
  = Message
  | EncKey

type alias Model = {
    rawStr : String,
    encKey: String,
    mode: InputMode,
    navKey : Nav.Key,
    url : Url.Url
  }

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = ({
    rawStr = "",
    encKey = "",
    mode = Message,
    navKey = key,
    url = url
  }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddLetter letter ->
      case model.mode of
        Message ->
          ({ model |
              rawStr =
                (if (String.length model.rawStr) < max_length then
                  model.rawStr ++ (filterInputLetter letter keys)
                else
                  model.rawStr
                )
          }, Cmd.none)
        _ ->
          ({ model |
              encKey =
                (if (String.length model.encKey) < max_length_key then
                  model.encKey ++ (filterInputLetter letter keysNum)
                else
                  model.encKey
                )
          }, Cmd.none)

    Control str ->
      (model, Cmd.none)

    Reset ->
      case model.mode of
        Message ->
          ({ model | rawStr = "" }, Cmd.none)
        _ ->
          ({ model | encKey = "" }, Cmd.none)

    BackspaceKey ->
      case model.mode of
        Message ->
          ({ model | rawStr = (String.dropRight 1 model.rawStr) }, Cmd.none)
        _ ->
          ({ model | encKey = (String.dropRight 1 model.encKey) }, Cmd.none)

    EnterKey ->
      case model.mode of
        Message
          -> ({model | mode = EncKey }, Cmd.none)
        _
          -> ({model | mode = Message,
                       rawStr = (encrypt model.rawStr model.encKey) },
              Cmd.none)

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.navKey (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }, Cmd.none )

filterInputLetter: String -> List String -> String
filterInputLetter letter allowedKeys =
  if List.any (String.contains letter) allowedKeys then
    letter
  else
    ""

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

remainLength : InputMode -> Model -> Int
remainLength mode model =
  if mode == Message then
    max_length - (String.length model.rawStr)
  else
    max_length_key - (String.length model.encKey)

renderTextAsList: String -> List (Html Msg)
renderTextAsList string =
  List.map  (\c -> div [class "letter"] [text (String.fromChar c)]) (String.toList string)

renderMessageInput: Model -> Html Msg
renderMessageInput model =
  div [ class (if model.mode == Message then "input" else "input flipped") ]
      (renderTextAsList model.rawStr)


renderEncKeyInput: Model -> Html Msg
renderEncKeyInput model =
  div [ class (if model.mode == Message then "input encKey flipped" else "input encKey") ]
      [
        div [] [ text "INPUT A KEY" ]
        , br [] []
        , text (model.encKey ++
                (if (remainLength EncKey model) > 0 then
                  (String.repeat (remainLength EncKey model) "_")
                  else
                  ""))
      ]

renderButtons: Model -> Html Msg
renderButtons model =
  let
    disableButtons =
      if model.mode == Message then
        (String.length model.rawStr) == 0
      else
        (String.length model.encKey) == 0

    disableEnter =
      if model.mode == Message then
        disableButtons
      else
        disableButtons || (String.length model.encKey) < max_length_key
  in
  div [ class "counter" ]
      [
        span [ class "number" ] [ text(String.fromInt (remainLength model.mode model)) ]
        , button [ class "backspace", onClick BackspaceKey, disabled disableButtons ] [ text "DEL" ]
        , button [ class ("reset"), onClick Reset, disabled disableButtons] [ text "CLR" ]
        , button [ class ("encrypt"), onClick EnterKey, disabled disableEnter] [ text "ENTER" ]
      ]

view : Model -> Browser.Document Msg
view model =
  { title = "My Secret Message"
  , body = [
      div [ class "main" ]
        [
          renderMessageInput model
          , renderEncKeyInput model
          , renderButtons model
          , renderKeyboard model
        ]
    ]
  }

