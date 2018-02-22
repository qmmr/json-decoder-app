module Main exposing (..)

import Html exposing (text)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (parseInt)


type Membership
    = Standard
    | Premium


type Gender
    = Female
    | Male


type alias User =
    { id : Int
    , email : String
    , membership : Membership
    , gender : Maybe Gender
    }


json =
    """
{
  "id" : "321",
  "email" : "Joe@doe.xyz",
  "isPremium" : true
}
"""


userDecoder =
    map4 User
        (field "id" parseInt)
        (field "email" (string |> map String.toLower))
        (field "isPremium" membership)
        (maybe (field "gender" gender))


gender : Decoder Gender
gender =
    let
        toGender : String -> Decoder Gender
        toGender str =
            case str of
                "male" ->
                    succeed Male

                "female" ->
                    succeed Female

                _ ->
                    fail (str ++ " is not a valid gender")
    in
        string |> andThen toGender


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
