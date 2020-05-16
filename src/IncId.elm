module IncId exposing
    ( IdDict
    , IncId
    , Seed
    , decoder
    , dictDecoder
    , dictEncoder
    , dictFromListBy
    , dictInsert
    , dictValues
    , encoder
    , initialSeed
    , next
    , seedDecoder
    , seedEncoder
    , toInt
    , toString
    )

import Dict
import Dict.Extra as Dict
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDX
import Json.Encode as JE exposing (Value)


type IncId
    = IncId Int


encoder : IncId -> Value
encoder (IncId id) =
    JE.object
        [ ( "tag", JE.string "IncId" )
        , ( "value", JE.int id )
        ]


decoder : Decoder IncId
decoder =
    JDX.when (JD.field "tag" JD.string)
        (\tag -> tag == "IncId")
        (JD.field "value" (JD.map IncId JD.int))


type Seed
    = Seed Int


initialSeed : Seed
initialSeed =
    Seed 1


seedEncoder : Seed -> Value
seedEncoder (Seed nextId) =
    JE.object
        [ ( "tag", JE.string "IncId.Seed" )
        , ( "value", JE.int nextId )
        ]


seedDecoder : Decoder Seed
seedDecoder =
    JDX.when (JD.field "tag" JD.string)
        (\tag -> tag == "IncId.Seed")
        (JD.field "value" (JD.map Seed JD.int))


next : Seed -> ( IncId, Seed )
next (Seed nextId) =
    ( IncId nextId, Seed (nextId + 1) )


toInt : IncId -> Int
toInt (IncId i) =
    i


toString : IncId -> String
toString =
    toInt >> String.fromInt


type IdDict a
    = IdDict (Dict.Dict Int a)


dictEncoder : (a -> Value) -> IdDict a -> Value
dictEncoder f (IdDict d) =
    JE.dict String.fromInt f d


dictDecoder : Decoder a -> Decoder (IdDict a)
dictDecoder =
    JDX.dict2 JD.int
        >> JD.map IdDict


dictFromListBy : (a -> IncId) -> List a -> IdDict a
dictFromListBy idFunc =
    Dict.fromListBy (idFunc >> toInt)
        >> IdDict


dictValues : IdDict a -> List a
dictValues (IdDict d) =
    Dict.values d


dictInsert : IncId -> a -> IdDict a -> IdDict a
dictInsert id a (IdDict d) =
    Dict.insert (toInt id) a d
        |> IdDict
