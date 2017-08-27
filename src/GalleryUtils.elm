module GalleryUtils exposing 
  ( makeDate
  , int2Time
  , makeUri
  , onNav
  , firstJust
  , justAccum
  , Action (..)
  )

import Time
import Date
import Http
import Html exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode

type Action 
    = DeleteDesign Int
    | EditDesign Int
    | AddFaves Int
    | RemoveFaves Int
    | DeleteComment Int
    | UpdateComment Int String
    | CreateComment Int String


int2Time : Int -> Time.Time
int2Time i = 
  (toFloat i) * 1000.0

makeDate : Time.Time -> String
makeDate udate =
  let
    d = Date.fromTime udate
    day = Date.day d
    suffix = 
      if day // 10 == 1 then
        "th"
      else
        case day % 10 of
          1 -> "st"
          2 -> "nd"
          3 -> "rd"
          _ -> "th"
    month = 
      case Date.month d of
          Date.Jan -> "January"
          Date.Feb -> "February"
          Date.Mar -> "March"
          Date.Apr -> "April"
          Date.May -> "May"
          Date.Jun -> "June"
          Date.Jul -> "July"
          Date.Aug -> "August"
          Date.Sep -> "September"
          Date.Oct -> "October"
          Date.Nov -> "November"
          Date.Dec -> "December"
  in
    month ++ " " ++ (toString day) ++ suffix ++ ", " ++ toString (Date.year d)


makeUri : String -> List String -> String
makeUri base rest =
  String.join "/" (base :: (List.map Http.encodeUri rest))

onNav : msg -> Attribute msg
onNav msg =
    onWithOptions "click" { stopPropagation = False, preventDefault = True } (Json.Decode.succeed msg)


{-mConcat : List (Maybe a) -> Maybe a
mConcat l =
  let
    firstJust ma mb = case ma of
      Just _ -> ma
      Nothing -> mb
  in
    List.foldl firstJust Nothing l-}

firstJust : List (Maybe a) -> Maybe a
firstJust l = case l of
  Just a :: _ -> Just a
  Nothing :: l_ -> firstJust l_
  [] -> Nothing

justAccum : Maybe a -> Maybe a -> Maybe a
justAccum ma mb = case ma of
  Just a -> ma
  Nothing -> mb

