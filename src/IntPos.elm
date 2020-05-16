module IntPos exposing (IntPos, decoder, encoder)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias IntPos =
    ( Int, Int )


encoder : IntPos -> Value
encoder intPos =
    (\( a, b ) -> JE.list identity [ JE.int a, JE.int b ]) intPos


decoder : Decoder IntPos
decoder =
    JD.map2 Tuple.pair (JD.index 0 JD.int) (JD.index 1 JD.int)
