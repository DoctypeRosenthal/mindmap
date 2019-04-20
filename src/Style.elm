module Style exposing (toStyles)

import Html
import Html.Attributes exposing (style)


toStyles : List ( String, String ) -> List (Html.Attribute msg)
toStyles list =
    List.map (\( key, val ) -> style key val) list
