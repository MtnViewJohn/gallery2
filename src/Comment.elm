module Comment exposing
  ( Comment
  , decodeComment
  , encodeComment
  , setupHtml

  , Msg (..)
  , update
  , view
  )

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline
import Time
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import GalleryUtils exposing (..)
import User exposing (..)
import Markdown
import Http exposing (encodeUri)

-- MODEL



type alias Comment =
    { comment : String
    , commentid : Int
    , postdate : Time.Time
    , screenname : String
    , htmltext : Html Msg
    }

options : Markdown.Options
options =
  let
    opts = Markdown.defaultOptions
  in
    { opts | githubFlavored = Just { tables = False, breaks = True }, 
             sanitize = True, 
             defaultHighlighting = Just "cfdg" }

toHtml : String -> Html Msg
toHtml = Markdown.toHtmlWith options []


int2Time : Int -> Time.Time
int2Time i = 
  (toFloat i) * 1000.0


decodeComment : Json.Decode.Decoder Comment
decodeComment =
    Json.Decode.Pipeline.decode Comment
        |> Json.Decode.Pipeline.required "comment" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "commentid" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "postdate" (Json.Decode.map int2Time Json.Decode.int)
        |> Json.Decode.Pipeline.required "screenname" (Json.Decode.string)
        |> Json.Decode.Pipeline.hardcoded (text "")

encodeComment : Comment -> Json.Encode.Value
encodeComment record =
    Json.Encode.object
        [ ("comment",     Json.Encode.string  <| record.comment)
        , ("commentid",   Json.Encode.int     <| record.commentid)
        , ("screenname",  Json.Encode.string  <| record.screenname)
        ]


setupHtml : Comment -> Comment
setupHtml comment = 
  { comment | htmltext = toHtml 
    (String.concat 
      [ comment.comment 
      , " - [**"
      , comment.screenname
      , "**](#user/"
      , encodeUri comment.screenname
      , ") on "
      , makeDate comment.postdate
      ]) }

-- UPDATE

type Msg
    = DeleteClick Int
    | EditClick Int

update : Msg -> Comment -> Comment
update msg comment =
  case msg of
    DeleteClick id ->
      comment  -- Container never sends this
    EditClick id->
      comment  -- Not implemented





-- VIEW

commentOwner : (Maybe User) -> String -> Bool
commentOwner loggedIn owner =
  case loggedIn of
    Nothing -> False
    Just user ->
      user.name == owner


view : (Maybe User) -> Comment -> Html Msg
view currentUser comment =
  div [class "commentblock", id ("comment" ++ toString comment.commentid)]
      [ comment.htmltext
      , if commentOwner currentUser comment.screenname then
          div []
          [ a [ href "#", onClick (DeleteClick comment.commentid), title "Delete this comment."] 
                    [ img [ src "graphics/deleteMiniButton.png", alt "Delete this comment",
                            width 30, height 22 ][]
                    ]
            , text " "
            , a [ href "#", onClick (EditClick comment.commentid), title "Edit this comment."] 
                [ img [ src "graphics/editMiniButton.png", alt "Edit this comment",
                        width 30, height 22 ][]
                ]
          ]
        else
          text ""
      ]