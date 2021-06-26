module LightBox exposing (..)

import Array
import Browser
import Element exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html)



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
    { images : List String
    , selectedImageSrc : String
    }


init : Array.Array String -> ( Model, Cmd Msg )
init images =
    let
        srcList =
            Array.toList images
    in
    ( { images = srcList, selectedImageSrc = initialSelectedImage srcList }
    , Cmd.none
    )


thumbSrcToFull : String -> String
thumbSrcToFull url =
    String.replace "-150x150" "" url


initialSelectedImage : List String -> String
initialSelectedImage images =
    Maybe.withDefault "" (List.head images)



-- UPDATE


type Msg
    = SelectedImage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedImage src ->
            ( { model | selectedImageSrc = src }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column []
            [ row []
                [ image
                    [ width fill
                    , height fill
                    ]
                    { src = thumbSrcToFull model.selectedImageSrc
                    , description = ""
                    }
                ]
            , wrappedRow []
                (List.map
                    (\source ->
                        image
                            [ width fill
                            , height fill
                            , onClick (SelectedImage source)
                            ]
                            { src = source
                            , description = ""
                            }
                    )
                    model.images
                )
            ]
