module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Node exposing (Node, nodeView)
import Task
import Time



---- MODEL ----


type alias Model =
    { root : Node
    , inEditMode : Bool
    }


init : ( Model, Cmd Node.Msg )
init =
    ( { root = Node.default, inEditMode = False }, Cmd.none )



---- UPDATE ----


update : Node.Msg -> Model -> ( Model, Cmd Node.Msg )
update msg model =
    case msg of
        Node.Edit _ ->
            if model.inEditMode then
                -- don't do anything
                ( model, Cmd.none )

            else
                ( { model | inEditMode = True, root = Node.update msg model.root }, Cmd.none )

        Node.GetTime callbackMsg ->
            ( model, Task.perform callbackMsg Time.now )

        _ ->
            ( { model | root = Node.update msg model.root }, Cmd.none )



---- VIEW ----


view : Model -> Html Node.Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , nodeView model.root
        ]



---- PROGRAM ----


main : Program () Model Node.Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
