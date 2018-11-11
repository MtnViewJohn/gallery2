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
  , Action (..)
  , TagInfo
  , TabStyle (..)
  , TabInfo
  , makeTabs
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
    month ++ " " ++ (String.fromInt day) ++ suffix ++ ", " ++ String.fromInt (Date.year d)


makeUri : String -> List String -> String
makeUri base rest =
  String.join "/" (base :: (List.map Http.encodeUri rest))

onNav : msg -> Attribute msg
onNav message =
    onWithOptions "click" { stopPropagation = False, preventDefault = True } (JD.succeed message)


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
    Empty -> "url(graphics/empty.png)"
    Active -> "url(graphics/active.png)"
    Inactive -> "url(graphics/inactive.png)"
    Rest -> "url(graphics/empty.png)"
    E2A -> "url(graphics/empty2active.png)"
    E2I -> "url(graphics/empty2inactive.png)"
    A2E -> "url(graphics/active2empty.png)"
    I2E -> "url(graphics/inactive2empty.png)"
    A2I -> "url(graphics/active2inactive.png)"
    I2A -> "url(graphics/inactive2active.png)"
    I2IL -> "url(graphics/inactive2inactive_left.png)"
    I2IR -> "url(graphics/inactive2inactive_right.png)"

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
    [ class <| styleClass tab.style
    , style [("background-image", styleImage tab.style)]
    ]
    [ if tab.url == "" then
        text <| toNB tab.content
      else
        a [href tab.url] [text <| toNB tab.content]
    ]

makeTabs : List TabInfo -> Html msg
makeTabs tabs =
  div [class "tabtable"]
    ( (List.map makeTab <| addInter tabs) ++ [ div [class "tabclear"] []] )
