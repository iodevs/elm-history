module History exposing (create)


type History a
    = History a (Maybe a)


create : a -> History a
create value =
    History value Nothing
