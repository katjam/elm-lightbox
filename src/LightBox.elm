module LightBox exposing (..)

import Array
import Browser
import Element exposing (..)
import Element.Border exposing (rounded)
import Element.Events exposing (onClick)
import Element.Input exposing (button)
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
    { imageList : List String
    , selectedImageSrc : String
    }


init : Array.Array String -> ( Model, Cmd Msg )
init images =
    let
        srcList =
            Array.toList images
    in
    ( { imageList = srcList, selectedImageSrc = initialSelectedImage srcList }
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
    | PressedPrevious
    | PressedNext


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
        column []
            [ row []
                [ button []
                    { onPress = Just PressedPrevious
                    , label = text "prev"
                    }
                , image
                    [ width fill
                    , height fill
                    , rounded 10
                    , clip
                    ]
                    { src = thumbSrcToFull model.selectedImageSrc
                    , description = ""
                    }
                , button []
                    { onPress = Just PressedNext
                    , label = text "next"
                    }
                ]
            , wrappedRow [ paddingXY 0 15, spacing 15 ]
                (List.map
                    (\source ->
                        image
                            [ width fill
                            , height fill
                            , rounded 10
                            , clip
                            , onClick (SelectedImage source)
                            ]
                            { src = source
                            , description = ""
                            }
                    )
                    model.imageList
                )
            ]
