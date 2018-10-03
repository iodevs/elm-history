module History exposing
    ( History
    , create, forward, rewind, current
    , rewindAll
    )

{-| This library helps with keeping history of states of your variables.
For example, you defined a width of some div (elm-css or style or ...) and
this width has been changed by user's action. After that you want to have
same width of div as before user's action.


# Definition

@docs History


# Helpers

@docs create, forward, rewind, current

-}


{-| Define data type.
A list contains history of changes which you want to keep.
-}
type History a
    = History a (List a)


{-| Create history for your a variable.

    import History exposing (History, create, forward, rewind)

    boxWidth : History String
    boxWidth =
        create "200px"


    -- creates: History "200px" []

-}
create : a -> History a
create value =
    History value []


{-| Adds newly incoming a variable and older puts to the list.


    addNew =
        forward "300px" boxWidth


    -- creates: History "300px" ["200px"]

-}
forward : a -> History a -> History a
forward value (History old past) =
    History value (old :: past)


{-| Inserts last added element of list to history.

    rewind addNew

    -- creates: History "200px" []

-}
rewind : History a -> History a
rewind ((History _ past) as history) =
    case past of
        old :: remains ->
            History old remains

        [] ->
            history


{-| Inserts first added element of list to history.

    rewindAll (History "1000px" ["800px", "600px", "400px", "200px"])

    -- creates: History "200px" []

-}
rewindAll : History a -> History a
rewindAll ((History _ past) as history) =
    case List.reverse past of
        old :: remains ->
            History old []

        [] ->
            history


{-| Gets current a value from history.

    addNew |> current --"300px"

    addNew |> rewind |> current --"200px"

-}
current : History a -> a
current (History cur _) =
    cur
