module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html)
import Node exposing (Node, nodeView)
import Svg.Attributes
import Task
import Time
import Svg

---- MODEL ----

type DragState
  = Static
  | Moving Int Int Int Int

type alias Model =
    { root : Node
    , dragState : DragState
    }


init : ( Model, Cmd Node.Msg )
init =
    ( { root = Node.default, dragState = Static }, Cmd.none )



---- UPDATE ----

update : Node.Msg -> Model -> ( Model, Cmd Node.Msg )
update msg model =
    case msg of
        Node.Edit _ ->
                ( { model | root = Node.update msg model.root }, Cmd.none )

        Node.GetTime callbackMsg ->
            ( model, Task.perform callbackMsg Time.now )

        _ ->
            ( { model | root = Node.update msg model.root }, Cmd.none )



---- VIEW ----


view : Model -> Html Node.Msg
view model =
    Svg.svg
        [ Svg.Attributes.width "120"
        , Svg.Attributes.height "120"
        , Svg.Attributes.viewBox "0 0 120 120"
        ]
        [ nodeView model.root ]



---- PROGRAM ----


main : Program () Model Node.Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }

