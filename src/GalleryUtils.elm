module GalleryUtils exposing 
  ( makeDate
  , int2Time
  , makeUri
  , onNav
  , tagHelp
  , DesignID (..)
  , nonDesign
  , idStr
  , CommentID (..)
  , noComment
  , cidStr
  , intStr
  , Action (..)
  , TagInfo
  )

import Time
import Date
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD

type DesignID = ID Int

nonDesign : DesignID
nonDesign = ID 0

idStr : DesignID -> String
idStr (ID num) =
  toString num

type CommentID = CID Int

noComment : CommentID
noComment = CID 0

cidStr : CommentID -> String
cidStr (CID num) =
  toString num

intStr : Int -> String
intStr num =
  toString num


type Action 
    = DeleteDesign DesignID
    | UploadDesign
    | EditDesign DesignID
    | AddFaves DesignID
    | Focus DesignID
    | RemoveFaves DesignID
    | DeleteComment CommentID
    | UpdateComment CommentID String
    | CreateComment String
    | CancelEditAct
    | CloseDesign
    | GetFile String

type alias TagInfo =
  { name : String
  , count : Int
  }



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
    month ++ " " ++ (intStr day) ++ suffix ++ ", " ++ intStr (Date.year d)


makeUri : String -> List String -> String
makeUri base rest =
  String.join "/" (base :: (List.map Http.encodeUri rest))

onNav : msg -> Attribute msg
onNav msg =
    onWithOptions "click" { stopPropagation = False, preventDefault = True } (JD.succeed msg)


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
