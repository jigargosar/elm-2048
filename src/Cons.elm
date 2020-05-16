module Cons exposing (Cons, fromList, fromTail, head, init, tail, toList)


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


head : Cons a -> a
head =
    Tuple.first


tail : Cons a -> List a
tail =
    Tuple.second


fromList : List a -> Maybe (Cons a)
fromList list =
    case list of
        [] ->
            Nothing

        a :: b ->
            Just (init a b)


fromTail : Cons a -> Maybe (Cons a)
fromTail =
    tail >> fromList


toList : Cons a -> List a
toList ( h, t ) =
    h :: t
