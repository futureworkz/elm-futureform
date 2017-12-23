module Form.Functions exposing (..)

import Date


{-| Return date or default date for a string
-}
dateFromStringWithDefault : String -> Date.Date
dateFromStringWithDefault value =
    Date.fromString value |> Result.withDefault (Date.fromTime 0)


{-| Return a new list string by add or remove item
-}
addOrRemoveListStringItem : List String -> String -> List String
addOrRemoveListStringItem list item =
    if List.member item list then
        List.filter (\str -> str /= item) list
    else
        List.append list [ item ]
