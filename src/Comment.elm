module Comment exposing
  ( Comment
  , decodeComment
  , encodeComment
  , setupHtml
  , setComment
  , isEditable
  , emptyComment

  , Msg (..)
  , MsgId
  , update
  , view
  )

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline
import Time
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import GalleryUtils exposing (..)
import User exposing (..)
import Markdown
import Http exposing (encodeUri)
--import Debug

-- MODEL


type DisplayType = Normal | Update | New

type alias Comment =
    { comment : String
    , commentmd : String
    , formcomment : String
    , commentid : Int
    , designid : Int
    , postdate : Time.Time
    , screenname : String
    , htmltext : Html Msg
    , displayMode: DisplayType
    , ready2delete : Bool
    }

emptyComment : Int -> Comment
emptyComment designid = 
  Comment "" "" "" 0 designid 0 "" (text "") New False

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
        |> Json.Decode.Pipeline.required "commentmd" (Json.Decode.string)
        |> Json.Decode.Pipeline.hardcoded ""
        |> Json.Decode.Pipeline.required "commentid" (Json.Decode.int)
        |> Json.Decode.Pipeline.hardcoded 0
        |> Json.Decode.Pipeline.required "postdate" (Json.Decode.map int2Time Json.Decode.int)
        |> Json.Decode.Pipeline.required "screenname" (Json.Decode.string)
        |> Json.Decode.Pipeline.hardcoded (text "")
        |> Json.Decode.Pipeline.hardcoded Normal
        |> Json.Decode.Pipeline.hardcoded False

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
      [ comment.commentmd 
      , " \\- [**"
      , comment.screenname
      , "**](#user/"
      , encodeUri comment.screenname
      , ") on "
      , makeDate comment.postdate
      ]) }

setComment : Int -> String -> Comment -> Comment
setComment id newStr oldComment =
  if id == oldComment.commentid then
    let
      newComment = { oldComment | comment = newStr }
    in
      setupHtml newComment
  else
    oldComment

isEditable : Comment -> Bool
isEditable c =
  c.displayMode == Update
        
-- UPDATE

type Msg
    = DeleteClick
    | CancelDeleteClick
    | EditClick
    | CancelClick
    | SubmitClick
    | NewText String

type alias MsgId = (Msg, Int)

update : Msg -> Comment -> (Comment, Maybe Action)
update msg cmt =
  case msg of
    DeleteClick ->
      if cmt.ready2delete then
        (cmt, Just (DeleteComment cmt.commentid))
      else
        ({cmt | ready2delete = True}, Nothing)
    CancelDeleteClick ->
      ({cmt | ready2delete = False}, Nothing)
    EditClick ->
        ({cmt | formcomment = cmt.comment, displayMode = Update}, Nothing)
    CancelClick ->
      if cmt.displayMode == New then
        ({cmt | formcomment = ""}, Nothing)
      else
        ({cmt | displayMode = Normal}, Nothing)
    SubmitClick ->
      -- Revert state and wait for the Action to produce a result
      if cmt.displayMode == New then
        let
          maction = Just (CreateComment cmt.designid cmt.formcomment)
        in
          ({cmt | formcomment = ""}, maction)
      else
        ({cmt | displayMode = Normal},
          Just (UpdateComment cmt.commentid cmt.formcomment))
    NewText new ->
        ({cmt | formcomment = new }, Nothing)




-- VIEW

commentOwner : (Maybe User) -> String -> Bool
commentOwner loggedIn owner =
  case loggedIn of
    Nothing -> False
    Just user ->
      user.name == owner

newTextMsg : Int -> String -> MsgId
newTextMsg id text =
  (NewText text, id)

newMsg : Int -> Msg -> MsgId
newMsg id msg =
  (msg, id)


view : (Maybe User) -> Comment -> Html MsgId
view currentUser comment =
  if comment.displayMode /= Normal then
    Html.form [onSubmit (SubmitClick, comment.commentid)]
    [ div [class "addcommentdiv", id "Addcommentdiv"]
      [ if comment.displayMode == New then
          text "Add a comment:"
        else
          text "Edit a comment:"
      , textarea [rows 5, cols 20, class "addcommenttext", value comment.formcomment,
          onInput (newTextMsg comment.commentid), name "comment"] []
      , p []
        [ text "CFDG code should be put between [code] ... [/code] tags to get formatted properly. " ]
      , p []
        [ text "Link tags should be in one of these three forms:" ]
      , ul []
        [ li [] [text "[link design:290]Random galaxies[/link]"]
        , li [] [text "[link user:IG14]other stuff by IG14[/link]"]
        , li [] [text "[link http://...]link to some other site[/link]"]
        ]
      , input [type_ "submit", value
        (if comment.displayMode == New then
          "Add comment"
        else
          "Update comment"), disabled (String.isEmpty comment.formcomment) ] []
      , input [type_ "submit", value "Cancel", onNav (CancelClick, comment.commentid)] []
      ]
    ]
  else
    div [class "commentblock", id ("comment" ++ toString comment.commentid)]
        [ Html.map (newMsg comment.commentid) comment.htmltext
        , if commentOwner currentUser comment.screenname then
            if comment.ready2delete then
              div []
              [ a [ href "#", onNav (DeleteClick, comment.commentid), title "Confirm deletion."] 
                      [ img [ src "graphics/mbtn_confirm.png", alt "Confirm deletion",
                              width 34, height 33 ][]
                      ]
              , text " "
              , a [ href "#", onNav (CancelDeleteClick, comment.commentid), title "Cancel deletion."] 
                  [ img [ src "graphics/mbtn_cancel.png", alt "Cancel deletion",
                          width 34, height 33 ][]
                  ]
              ]
            else
              div []
              [ a [ href "#", onNav (DeleteClick, comment.commentid), title "Delete this comment."] 
                      [ img [ src "graphics/mbtn_delete.png", alt "Delete this comment",
                              width 34, height 33 ][]
                      ]
              , text " "
              , a [ href "#", onNav (EditClick, comment.commentid), title "Edit this comment."] 
                  [ img [ src "graphics/mbtn_edit.png", alt "Edit this comment",
                          width 34, height 33 ][]
                  ]
              ]
          else
            text ""
        ]
