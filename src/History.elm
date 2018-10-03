module History exposing (create, current, forward, rewind)


type History a
    = History a (List a)


create : a -> History a
create value =
    History value []


forward : a -> History a -> History a
forward value (History old past) =
    History value (old :: past)


rewind : History a -> History a
rewind ((History _ past) as history) =
    case past of
        old :: remains ->
            History old remains

        [] ->
            history


current : History a -> a
current (History cur _) =
    cur
