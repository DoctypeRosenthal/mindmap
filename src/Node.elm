module Node exposing (Msg(..), Node, default, nodeView, update)

import Color exposing (Color, black, blue, colorToStyle)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (keyCode, onClick)
import Json.Decode as Decode
import Svg
import Svg.Attributes as SvgAtt
import Time


onKeyDown : msg -> Html.Attribute msg
onKeyDown message =
    Html.Events.custom "keydown" (Decode.succeed {message=message,stopPropagation=True,preventDefault=True} )

onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
  Html.Events.on "change" <| Decode.map handler <| Decode.at ["target", "value"] Decode.string

type Node
    = Node
        { id : Int
        , x : Int
        , y : Int
        , inEditMode : Bool
        , fontSize : Int
        , fontWeight : Int
        , children : List Node
        , color : Color
        , isFilled : Bool
        , text : String
        }


default : Node
default =
    Node
        { id = 0
        , x = 100
        , y = 100
        , inEditMode = False
        , fontSize = 12
        , fontWeight = 600
        , children = []
        , color = blue
        , isFilled = False
        , text = "Mindmap"
        }


withCoordinates : Int -> Int -> Node -> Node
withCoordinates x y (Node n) =
    Node { n | x = x, y = y }


type Msg
    = GetTime (Time.Posix -> Msg)
    | Edit Node
    | SetText Node String
    | CreateChildFor Node
    | Delete Node


update : Msg -> Node -> Node
update msg root =
    case msg of
        Edit node ->
            findAndUpdate node (setEditMode True node) root

        CreateChildFor node ->
            findAndUpdate node (addChild node) root

        Delete node ->
            node

        SetText node string ->
            let nextNode = node
                    |> setText string
                    |> setEditMode False
            in
            findAndUpdate node (nextNode) root

        GetTime _ ->
            root


findAndUpdate : Node -> Node -> Node -> Node
findAndUpdate  node updatedNode((Node r) as root) =
    if node == root then
        updatedNode

    else
        Node { r | children = List.map (findAndUpdate node updatedNode ) r.children }


addChild :  Node -> Node
addChild  (Node n) =
    let
        childCount =
            List.length n.children

        new =
            default
                |> withCoordinates (n.x + (childCount * 100)) (n.y + 50)
    in
    Node { n | children = new :: n.children }


setEditMode : Bool -> Node -> Node
setEditMode enableEdit (Node n) =
    Node { n | inEditMode = enableEdit }


setText : String -> Node -> Node
setText string (Node n) =
    Node { n | text = string }


nodeView : Node -> Html Msg
nodeView ((Node n) as node) =
    Svg.rect
        [ SvgAtt.x <| String.fromInt n.x
        , SvgAtt.y <| String.fromInt n.y
        , style "border" <| "2px solid " ++ colorToStyle n.color
        , style "color" <| colorToStyle n.color
        , style "font-weight" <| String.fromInt n.fontWeight
        , SvgAtt.class "node"
        ]
        ([ toolsView node
         , if not n.inEditMode then
            Html.span
                [ Html.Attributes.class "node__text"
                , Html.Events.onDoubleClick <| Edit node
                ]
                [Html.text n.text]

           else
            Html.input [ Html.Attributes.value n.text, onChange <| SetText node ] []
         ]
            ++ List.map nodeView n.children
        )


toolsView : Node -> Html Msg
toolsView node =
    Html.div
        [ Html.Attributes.class "node__tools" ]
        [ Html.button [ onClick <| CreateChildFor node ] [ Html.text "+" ] ]
