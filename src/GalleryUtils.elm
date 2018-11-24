module GalleryUtils exposing 
  ( makeDate
  , int2Time
  , makeUri
  , onNav
  , tagHelp
  , DesignID (..)
  , nonDesign
  , idStr
  , possessive
  , CommentID (..)
  , noComment
  , cidStr
  , Action (..)
  , TagInfo
  , TabStyle (..)
  , TabInfo
  , makeTabs
  , decodeFiles
  )

import Time
import Url
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as JD
import File

type DesignID = ID Int

nonDesign : DesignID
nonDesign = ID 0

idStr : DesignID -> String
idStr (ID num) =
  String.fromInt num

type CommentID = CID Int

noComment : CommentID
noComment = CID 0

cidStr : CommentID -> String
cidStr (CID num) =
  String.fromInt num


type Action 
    = DeleteDesign DesignID
    | UploadDesign
    | UploadDesignTags
    | EditDesign DesignID
    | AddFaves DesignID
    | Focus DesignID
    | Translate DesignID
    | RemoveFaves DesignID
    | DeleteComment CommentID
    | UpdateComment CommentID String
    | CreateComment String
    | CancelEditAct
    | CloseDesign
    | ShowFans Bool

type alias TagInfo =
  { name : String
  , count : Int
  }


possessive : String -> String
possessive name =
  if String.endsWith "s" name then
    name ++ "'"
  else
    name ++ "'s"

int2Time : Int -> Time.Posix
int2Time i = 
  Time.millisToPosix (i * 1000)

makeDate : Time.Posix -> String
makeDate udate =
  let
    day = Time.toDay Time.utc udate
    suffix = 
      if day // 10 == 1 then
        "th"
      else
        case modBy day 10 of
          1 -> "st"
          2 -> "nd"
          3 -> "rd"
          _ -> "th"
    month = 
      case Time.toMonth Time.utc udate of
          Time.Jan -> "January"
          Time.Feb -> "February"
          Time.Mar -> "March"
          Time.Apr -> "April"
          Time.May -> "May"
          Time.Jun -> "June"
          Time.Jul -> "July"
          Time.Aug -> "August"
          Time.Sep -> "September"
          Time.Oct -> "October"
          Time.Nov -> "November"
          Time.Dec -> "December"
  in
    month ++ " " ++ (String.fromInt day) ++ suffix ++ ", " ++ String.fromInt (Time.toYear Time.utc udate)


makeUri : String -> List String -> String
makeUri base rest =
  String.join "/" (base :: (List.map Url.percentEncode rest))

onNav : msg -> Attribute msg
onNav message =
  preventDefaultOn "click" (JD.map alwaysPreventDefault (JD.succeed message)) 

alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
  ( msg, True )

decodeFiles : JD.Decoder (List File.File)
decodeFiles =
  JD.at ["target","files"] (JD.list File.decoder)


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

type TabStyle = Empty | Active | Inactive | E2A | E2I | A2E | I2E | A2I | I2A | I2IL | I2IR | Rest

type alias TabInfo =
  { style : TabStyle
  , content : String
  , url : String
  }

    -- Map spaces to non-breaking spaces
toNB : String -> String
toNB str = String.map (\c -> if c == ' ' then ' ' else c) str

interStyle : TabStyle -> TabStyle -> TabStyle
interStyle a b =
  case (a, b) of
    (Empty,Active) -> E2A
    (Empty,Inactive) -> E2I
    (Active,Empty) -> A2E
    (Inactive,Empty) -> I2E
    (Active,Inactive) -> A2I
    (Inactive,Active) -> I2A
    (Inactive,Inactive) -> I2IL   -- TODO: figure out left or right
    (Empty,Rest) -> E2I
    (Active,Rest) -> A2E
    (Inactive,Rest) -> I2E
    _ -> I2IR

styleImage : TabStyle -> String
styleImage style =
  case style of
    Empty -> " Empty"
    Active -> " Active"
    Inactive -> " Inactive"
    Rest -> " Empty"
    E2A -> " E2A"
    E2I -> " E2I"
    A2E -> " A2E"
    I2E -> " I2E"
    A2I -> " A2I"
    I2A -> " I2A"
    I2IL -> " I2IL"
    I2IR -> " I2IR"

styleClass : TabStyle -> String
styleClass style =
  case style of
    Empty -> "tabdata"
    Active -> "tabdata"
    Inactive -> "tabdata"
    Rest -> "tabrest rightcell"
    _ -> "tabinter"

addInter : List TabInfo -> List TabInfo
addInter tabs =
  case tabs of
    a :: b :: rest -> a :: (TabInfo (interStyle a.style b.style) " " "") 
                        :: (addInter <| b :: rest)
    _ -> tabs

makeTab : TabInfo -> Html msg
makeTab tab =
  div 
    [ class <| styleClass tab.style ++ (styleImage tab.style)
    ]
    [ if tab.url == "" then
        text <| toNB tab.content
      else
        a [href tab.url] [text <| toNB tab.content]
    ]

makeTabs : List TabInfo -> Html msg
makeTabs tabs =
  div [class "tabtable"]
    (List.map makeTab <| addInter tabs)
