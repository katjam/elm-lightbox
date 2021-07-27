module LightBox exposing (..)

import Browser
import Element exposing (clip, column, fill, layout, paddingEach, paddingXY, px, rgb255, row, spacing, wrappedRow)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Events exposing (onClick)
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as D
import Svg exposing (svg)
import Svg.Attributes exposing (d, stroke, viewBox)
import Svg.Events exposing (onMouseOut, onMouseOver)
import Theme exposing (color)



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
    { imageList : List ImageData
    , selectedImageData : ImageData
    , previousColor : String
    , nextColor : String
    }


init : D.Value -> ( Model, Cmd Msg )
init imageList =
    let
        srcList =
            case D.decodeValue listOfImageDataDecoder imageList of
                Ok goodImageData ->
                    goodImageData

                Err _ ->
                    [ { thumbSrc = ""
                      , fullSrc = ""
                      }
                    ]
    in
    ( { imageList = srcList
      , selectedImageData = initialSelectedImage srcList
      , previousColor = color.blueHex
      , nextColor = color.blueHex
      }
    , Cmd.none
    )


type alias ImageData =
    { thumbSrc : String
    , fullSrc : String
    }


imageDataDecoder : D.Decoder ImageData
imageDataDecoder =
    D.map2
        ImageData
        (D.field "thumbSrc" D.string)
        (D.field "fullSrc" D.string)


listOfImageDataDecoder : D.Decoder (List ImageData)
listOfImageDataDecoder =
    D.list imageDataDecoder


initialSelectedImage : List ImageData -> ImageData
initialSelectedImage imageList =
    Maybe.withDefault { thumbSrc = "", fullSrc = "" } (List.head imageList)



-- UPDATE


type Msg
    = SelectedImage ImageData
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
        SelectedImage imageData ->
            ( { model | selectedImageData = imageData }
            , Cmd.none
            )

        PressedPrevious ->
            let
                previousImgData =
                    getPreviousImage model
            in
            ( { model | selectedImageData = previousImgData }, Cmd.none )

        PressedNext ->
            let
                nextImgData =
                    getNextImage model
            in
            ( { model | selectedImageData = nextImgData }, Cmd.none )

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


getImageWithNeighbours : (ImageData -> Bool) -> List ImageData -> ( Maybe ImageData, Maybe ImageData, Maybe ImageData )
getImageWithNeighbours pred imageList =
    case imageList of
        prevImage :: currentImage :: nextImage :: rest ->
            if pred currentImage then
                ( Just prevImage, Just currentImage, Just nextImage )

            else if pred prevImage then
                ( Nothing, Just prevImage, Just currentImage )

            else
                getImageWithNeighbours pred (currentImage :: nextImage :: rest)

        prevImage :: currentImage :: [] ->
            if pred prevImage then
                ( Nothing, Just prevImage, Just currentImage )

            else if pred currentImage then
                ( Just prevImage, Just currentImage, Nothing )

            else
                getImageWithNeighbours pred [ currentImage ]

        prevImage :: [] ->
            if pred prevImage then
                ( Nothing, Just prevImage, Nothing )

            else
                ( Nothing, Nothing, Nothing )

        [] ->
            ( Nothing, Nothing, Nothing )


getPreviousImage : Model -> ImageData
getPreviousImage model =
    let
        imageWithNeighbours =
            getImageWithNeighbours (\src -> src == model.selectedImageData) model.imageList

        ( prevImage, _, _ ) =
            imageWithNeighbours
    in
    case prevImage of
        Just imageData ->
            imageData

        Nothing ->
            model.selectedImageData


getNextImage : Model -> ImageData
getNextImage model =
    let
        imageWithNeighbours =
            getImageWithNeighbours (\imageData -> imageData == model.selectedImageData) model.imageList

        ( _, _, nextImageData ) =
            imageWithNeighbours
    in
    case nextImageData of
        Just imageData ->
            imageData

        Nothing ->
            model.selectedImageData



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ Element.width (fill |> Element.maximum 1200)
        ]
    <|
        column
            [ Background.color color.grey
            , paddingEach { top = 20, right = 28, bottom = 0, left = 28 }
            ]
            [ row
                [ Element.spacing 20
                ]
                [ button [ Element.width <| px 40 ]
                    { onPress = Just PressedPrevious
                    , label = Element.html (prevSvg model.previousColor)
                    }
                , Element.image
                    [ Element.width fill
                    , Element.height fill
                    , rounded 8
                    , clip
                    ]
                    { src = model.selectedImageData.fullSrc
                    , description = ""
                    }
                , button [ Element.width <| px 40 ]
                    { onPress = Just PressedNext
                    , label = Element.html (nextSvg model.nextColor)
                    }
                ]
            , wrappedRow [ paddingXY 0 15, spacing 15 ]
                (List.map
                    (\imageData ->
                        Element.image
                            [ Element.width (fill |> Element.minimum 150 |> Element.maximum 150)
                            , Element.height fill
                            , rounded 8
                            , clip
                            , onClick (SelectedImage imageData)
                            ]
                            { src = imageData.thumbSrc
                            , description = ""
                            }
                    )
                    model.imageList
                )
            ]


prevSvg : String -> Html Msg
prevSvg colorState =
    svg
        [ viewBox "0 0 19 28"
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


nextSvg : String -> Html Msg
nextSvg colorState =
    svg
        [ viewBox "0 0 19 28"
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
