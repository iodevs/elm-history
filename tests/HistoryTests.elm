module HistoryTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import History exposing (History, create, current, forward, rewind)
import Test exposing (..)


suite : Test
suite =
    describe "The History module"
        [ describe "create"
            [ fuzz int "new history" <|
                \value ->
                    let
                        h =
                            create value
                    in
                    Expect.equal (h |> current) value
            ]
        , describe "forward and rewind"
            [ fuzz (list string) "should store history values and get it in reverse order (heap)" <|
                \vals ->
                    vals
                        |> List.foldl forward (create "first")
                        |> compare vals
                        |> Expect.true "Expected the history store events in reverse order"
            , test "should stop on end" <|
                \_ ->
                    let
                        h =
                            create "end"

                        first =
                            rewind h

                        second =
                            rewind first
                    in
                    Expect.equal first second
            ]
        , describe "current"
            [ fuzz (list string) "should return current value" <|
                \vals ->
                    let
                        h =
                            vals
                                |> List.foldl forward (create "first")
                                |> forward "last"
                    in
                    Expect.equal (current h) "last"
            ]
        ]


compare : List String -> History String -> Bool
compare vals hs =
    let
        f val ( h, res ) =
            ( rewind h, res && (current h == val) )
    in
    vals
        |> List.foldr f ( hs, True )
        |> (\( _, result ) -> result)
