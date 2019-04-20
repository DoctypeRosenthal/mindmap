module Node exposing (Msg(..), Node, default, nodeView, update)

import Color exposing (Color, black, blue, colorToStyle)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (keyCode, onClick)
import Json.Decode as Decode
import Time


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (Decode.map tagger keyCode)


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
    | FinishEdit Node Int
    | CreateChildFor Node Time.Posix
    | Delete Node


update : Msg -> Node -> Node
update msg root =
    let
        recurseUpdate =
            findAndUpdate root msg
    in
    case msg of
        Edit node ->
            recurseUpdate node (setEditMode True)

        FinishEdit node kCode ->
            if kCode /= 13 then
                node

            else
                -- Enter pressed
                recurseUpdate node (setEditMode False)

        CreateChildFor node time ->
            recurseUpdate node (addChild <| Time.posixToMillis time)

        Delete node ->
            node

        SetText node string ->
            recurseUpdate node (setText string)

        GetTime _ ->
            root


findAndUpdate : Node -> Msg -> Node -> (Node -> Node) -> Node
findAndUpdate ((Node r) as root) msg node f =
    if node == root then
        f root

    else if List.member node r.children then
        Node { r | children = List.map (update msg) r.children }

    else
        root


addChild : Int -> Node -> Node
addChild id (Node n) =
    let
        childCount =
            List.length n.children

        new =
            default
                |> withId id
                |> withCoordinates (n.x + (childCount * 100)) (n.y + 50)
    in
    Node { n | children = new :: n.children }


withId : Int -> Node -> Node
withId int (Node n) =
    Node { n | id = int }


setEditMode : Bool -> Node -> Node
setEditMode enableEdit (Node n) =
    Node { n | inEditMode = enableEdit }


setText : String -> Node -> Node
setText string (Node n) =
    Node { n | text = string }


nodeView : Node -> Html Msg
nodeView ((Node n) as node) =
    Html.div
        [ style "top" <| String.fromInt n.y ++ "px"
        , style "left" <| String.fromInt n.y ++ "px"
        , style "border" <| "2px solid " ++ colorToStyle n.color
        , style "color" <| colorToStyle n.color
        , style "font-weight" <| String.fromInt n.fontWeight
        , Html.Attributes.class "node"
        , Html.Events.onDoubleClick <| Edit node
        ]
        ([ toolsView node
         , if not n.inEditMode then
            Html.text n.text

           else
            Html.input [ Html.Attributes.value n.text, Html.Events.onInput <| SetText node, onKeyDown <| FinishEdit node ] []
         ]
            ++ List.map nodeView n.children
        )


toolsView : Node -> Html Msg
toolsView node =
    Html.div
        [ Html.Attributes.class "node__tools" ]
        [ Html.button [ onClick <| GetTime <| CreateChildFor node ] [ Html.text "+" ] ]
