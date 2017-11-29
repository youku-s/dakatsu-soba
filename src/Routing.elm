module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (..)
import UrlParser exposing (..)

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [
            map NewDetail top,
            map Detail (s "detail" </> string),
            map NewDetail (s "new")
        ]

parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
