module Stack exposing (Stack, empty, pop, push)


type alias Stack a =
    List a


empty : Stack a
empty =
    []


push : a -> Stack a -> Stack a
push item stack =
    item :: stack


pop : Stack a -> Maybe ( a, Stack a )
pop stack =
    case stack of
        [] ->
            Nothing

        item :: stack ->
            Just ( item, stack )
