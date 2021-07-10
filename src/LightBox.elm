module LightBox exposing (..)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events exposing (onClick)
import Element.Input exposing (button)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseOut, onMouseOver)



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
    { imageList : List String
    , selectedImageSrc : String
    , previousColor: String
    , nextColor: String
    }


init : Array.Array String -> ( Model, Cmd Msg )
init images =
    let
        srcList =
            Array.toList images
    in
    (
    { imageList = srcList
    , selectedImageSrc = initialSelectedImage srcList 
    , previousColor = color.blueHex
    , nextColor = color.blueHex
    }
    , Cmd.none
    )


thumbSrcToFull : String -> String
thumbSrcToFull url =
    String.replace "-150x150" "" url


initialSelectedImage : List String -> String
initialSelectedImage imageList =
    Maybe.withDefault "" (List.head imageList)



-- UPDATE


type Msg
    = SelectedImage String
    | MouseOver Mouseable
    | MouseOut Mouseable
    | PressedPrevious
    | PressedNext

type Mouseable
   = PreviousArrow
   | NextArrow

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedImage src ->
            ( { model | selectedImageSrc = src }
            , Cmd.none
            )

        PressedPrevious ->
            let
                previousImgSource =
                    getPreviousSrc model
            in
            ( { model | selectedImageSrc = previousImgSource }, Cmd.none )

        PressedNext ->
            let
                nextImgSource =
                    getNextSrc model
            in
            ( { model | selectedImageSrc = nextImgSource }, Cmd.none )
        MouseOver control ->
            case control of
                PreviousArrow ->
                    ( { model | previousColor = color.lightBlueHex }, Cmd.none )
                NextArrow ->
                    ( { model | nextColor = color.lightBlueHex }, Cmd.none )
        MouseOut control ->
            case control of
                PreviousArrow ->
                    ( { model | previousColor = color.blueHex }, Cmd.none )
                NextArrow ->
                    ( { model | nextColor = color.blueHex }, Cmd.none )



-- HELPERS


getImageWithNeighbours : (String -> Bool) -> List String -> ( Maybe String, Maybe String, Maybe String )
getImageWithNeighbours pred imageList =
    case imageList of
        prevSrc :: currentSrc :: nextSrc :: rest ->
            if pred currentSrc then
                ( Just prevSrc, Just currentSrc, Just nextSrc )

            else if pred prevSrc then
                ( Nothing, Just prevSrc, Just currentSrc )

            else
                getImageWithNeighbours pred (currentSrc :: nextSrc :: rest)

        prevSrc :: currentSrc :: [] ->
            if pred prevSrc then
                ( Nothing, Just prevSrc, Just currentSrc )

            else if pred currentSrc then
                ( Just prevSrc, Just currentSrc, Nothing )

            else
                getImageWithNeighbours pred [ currentSrc ]

        prevSrc :: [] ->
            if pred prevSrc then
                ( Nothing, Just prevSrc, Nothing )

            else
                ( Nothing, Nothing, Nothing )

        [] ->
            ( Nothing, Nothing, Nothing )


getPreviousSrc : Model -> String
getPreviousSrc model =
    let
        imageWithNeighbours =
            getImageWithNeighbours (\src -> src == model.selectedImageSrc) model.imageList

        ( prevImageSrc, _, _ ) =
            imageWithNeighbours
    in
    case prevImageSrc of
        Just src ->
            src

        Nothing ->
            model.selectedImageSrc


getNextSrc : Model -> String
getNextSrc model =
    let
        imageWithNeighbours =
            getImageWithNeighbours (\src -> src == model.selectedImageSrc) model.imageList

        ( _, _, nextImageSrc ) =
            imageWithNeighbours
    in
    case nextImageSrc of
        Just src ->
            src

        Nothing ->
            model.selectedImageSrc



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ Background.color color.grey
            , paddingXY 10 20
            ]
            [ row []
                [ button [ Element.width <| px 45 ]
                    { onPress = Just PressedPrevious
                    , label = Element.html (prevSvg model.previousColor)
                    }
                , Element.image
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , rounded 8
                    , Element.clip
                    ]
                    { src = thumbSrcToFull model.selectedImageSrc
                    , description = ""
                    }
                , button [ Element.width <| px 45 ]
                    { onPress = Just PressedNext
                    , label = Element.html (nextSvg model.nextColor)
                    }
                ]
            , wrappedRow [ paddingXY 0 15, Element.spacing 15 ]
                (List.map
                    (\source ->
                        Element.image
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , rounded 8
                            , Element.clip
                            , onClick (SelectedImage source)
                            ]
                            { src = source
                            , description = ""
                            }
                    )
                    model.imageList
                )
            ]


color =
    { grey = rgb255 211 211 211
    , blueHex = "#020d4c"
    , lightBlueHex = "#405cd4"
    }


prevSvg colorState =
    svg
        [ Svg.Attributes.width "40"
        , viewBox "0 0 19 28"
        , onMouseOver (MouseOver PreviousArrow)
        , onMouseOut (MouseOut PreviousArrow)
        ]
        [ Svg.path
            [ stroke colorState
            , Svg.Attributes.fill colorState
            , d "M18.297 4.703l-8.297 8.297 8.297 8.297c0.391 0.391 0.391 1.016 0 1.406l-2.594 2.594c-0.391 0.391-1.016 0.391-1.406 0l-11.594-11.594c-0.391-0.391-0.391-1.016 0-1.406l11.594-11.594c0.391-0.391 1.016-0.391 1.406 0l2.594 2.594c0.391 0.391 0.391 1.016 0 1.406z"
            ]
            []
        ]


nextSvg colorState =
    svg
        [ Svg.Attributes.width "40"
        , viewBox "0 0 19 28"
        , onMouseOver (MouseOver NextArrow)
        , onMouseOut (MouseOut NextArrow)
        ]
        [ Svg.path
            [ stroke colorState
            , Svg.Attributes.fill colorState
            , d "M17.297 13.703l-11.594 11.594c-0.391 0.391-1.016 0.391-1.406 0l-2.594-2.594c-0.391-0.391-0.391-1.016 0-1.406l8.297-8.297-8.297-8.297c-0.391-0.391-0.391-1.016 0-1.406l2.594-2.594c0.391-0.391 1.016-0.391 1.406 0l11.594 11.594c0.391 0.391 0.391 1.016 0 1.406z"
            ]
            []
        ]
