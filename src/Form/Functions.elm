module Form.Functions exposing (..)

import Date


{-| Return date or default date for a string
-}
dateFromStringWithDefault : String -> Date.Date
dateFromStringWithDefault value =
    Date.fromString value |> Result.withDefault (Date.fromTime 0)
