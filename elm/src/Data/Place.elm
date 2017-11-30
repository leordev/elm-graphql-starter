module Data.Place exposing (Place, Slug, slugParser, slugToString)

import UrlParser


type alias PlaceId =
    String


type alias Place =
    { id : PlaceId
    , slug : Slug
    , name : String
    , description : String
    , latitude : Float
    , longitude : Float
    }



-- IDENTIFIERS --


type Slug
    = Slug String


slugParser : UrlParser.Parser (Slug -> a) a
slugParser =
    UrlParser.custom "SLUG" (Ok << Slug)


slugToString : Slug -> String
slugToString (Slug slug) =
    slug
