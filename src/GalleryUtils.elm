module GalleryUtils exposing 
  ( makeDate
  , int2Time
  , makeUri
  , onNav
  , firstJust
  , justAccum
  , tagHelp
  , Action (..)
  )

import Time
import Date
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD

type Action 
    = DeleteDesign Int
    | UploadDesign
    | EditDesign Int
    | AddFaves Int
    | RemoveFaves Int
    | DeleteComment Int
    | UpdateComment Int String
    | CreateComment Int String
    | CancelEditAct
    | GetFile String


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
    onWithOptions "click" { stopPropagation = False, preventDefault = True } (JD.succeed msg)


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

tagHelp : Html msg
tagHelp =
  div [class "taghelp"]
    [ p [] [text "CFDG code should be put between [code] ... [/code] tags to get formatted properly. "]
    , p [] 
      [ text "Formatting using "
      , a [ href "https://help.github.com/articles/basic-writing-and-formatting-syntax/"
          , target "_blank"
          ] 
          [text "Github-flavored Markdown"]
      , text " is also supported. This means that you must \\-escape markdown characters in your text."
      ]
    , p [] [text "Link tags should be in one of these three forms:"]
    , ul []
      [ li [] [text "[link design:505]Decreasing Aperture[/link]"]
      , li [] [text "[link user:Sykil]other stuff by Sykil[/link]"]
      , li [] [text "[link http://...]link to some other site[/link]"]
      ]
    ]
