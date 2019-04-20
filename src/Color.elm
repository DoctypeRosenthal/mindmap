module Color exposing (Byte(..), Color, black, byte, byteToStr, colorToStyle, blue)


type Byte
    = Byte Int


byteToStr : Byte -> String
byteToStr (Byte int) =
    String.fromInt int


byte : Int -> Byte
byte int =
    Byte <| clamp 0 255 int


type alias Color =
    { r : Byte, g : Byte, b : Byte }


colorToStyle : Color -> String
colorToStyle { r, g, b } =
    "rgb(" ++ byteToStr r ++ "," ++ byteToStr g ++ "," ++ byteToStr b ++ ")"


black : Color
black =
    let
        zero =
            byte 0
    in
    Color zero zero zero


blue : Color
blue =
    Color (byte 0) (byte 0) (byte 255)
