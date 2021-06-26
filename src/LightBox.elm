module LightBox exposing (..)
import Browser
import Html exposing (..)


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { text1: String
  , text2: String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {text1 = "hello", text2= "world"}
  , Cmd.none
  )



-- UPDATE


type Msg
  = None



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
      ( { model | text1 = "hej"}
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  h1 [] [text model.text1, text model.text2 ]
