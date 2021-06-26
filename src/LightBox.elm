module LightBox exposing (..)

import Array
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
    { images : Array.Array String
    }


init : Array.Array String -> ( Model, Cmd Msg )
init images =
    ( { images = images }
    , Cmd.none
    )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map (\src -> text src) (Array.toList model.images))
