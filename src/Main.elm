module Main exposing (..)

import Html exposing (text)
import Json.Decode exposing (..)


type Membership
    = Standard
    | Premium


type alias User =
    { id : Int
    , email : String
    , membership : Membership
    }


json =
    """
{
  "id" : 123,
  "email" : "Joe@domain.net",
  "isPremium" : true
}
"""


userDecoder =
    map3
        User
        (field "id" int)
        (field "email" (string |> map String.toLower))
        (field "isPremium" membership)


membership =
    let
        toMembership b =
            case b of
                True ->
                    Premium

                False ->
                    Standard
    in
        bool |> map toMembership


main =
    json
        |> decodeString userDecoder
        |> toString
        |> text
