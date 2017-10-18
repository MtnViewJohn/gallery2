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

import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JPipe
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
    , commentid : CommentID
    , postdate : Time.Time
    , screenname : String
    , htmltext : Html Msg
    , displayMode: DisplayType
    , ready2delete : Bool
    }

emptyComment : Comment
emptyComment = 
  Comment "" "" "" noComment 0 "" (text "") New False

options : Markdown.Options
options =
  let
    opts = Markdown.defaultOptions
  in
    { opts | githubFlavored = Just { tables = False, breaks = True }
           , sanitize = True
           , defaultHighlighting = Just "cfdg" }

toHtml : String -> Html Msg
toHtml = Markdown.toHtmlWith options []


int2Time : Int -> Time.Time
int2Time i = 
  (toFloat i) * 1000.0


decodeComment : JD.Decoder Comment
decodeComment =
    JPipe.decode Comment
        |> JPipe.required "comment" (JD.string)
        |> JPipe.required "commentmd" (JD.string)
        |> JPipe.hardcoded ""
        |> JPipe.required "commentid" (JD.map CID JD.int)
        |> JPipe.required "postdate" (JD.map int2Time JD.int)
        |> JPipe.required "screenname" (JD.string)
        |> JPipe.hardcoded (text "")
        |> JPipe.hardcoded Normal
        |> JPipe.hardcoded False

encodeComment : Comment -> JE.Value
encodeComment record =
  case record.commentid of
    CID cid ->
      JE.object
          [ ("comment",     JE.string  <| record.comment)
          , ("commentid",   JE.int     <| cid)
          , ("screenname",  JE.string  <| record.screenname)
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

setComment : CommentID -> String -> Comment -> Comment
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

type alias MsgId = (Msg, CommentID)

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
          maction = Just (CreateComment cmt.formcomment)
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

newTextMsg : CommentID -> String -> MsgId
newTextMsg id text =
  (NewText text, id)

newMsg : CommentID -> Msg -> MsgId
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
      , tagHelp
      , input [type_ "submit", value
        (if comment.displayMode == New then
          "Add comment"
        else
          "Update comment"), disabled (String.isEmpty comment.formcomment) ] []
      , input [type_ "submit", value "Cancel", onNav (CancelClick, comment.commentid)] []
      ]
    ]
  else
    div [class "commentblock", id ("comment" ++ cidStr comment.commentid)]
        [ Html.map (newMsg comment.commentid) comment.htmltext
        , if commentOwner currentUser comment.screenname then
            if comment.ready2delete then
              div []
              [ a [ href "#", onNav (CancelDeleteClick, comment.commentid), title "Cancel deletion."
                  , class "keepbutton"
                  ] [ ]
              , text " "
              , a [ href "#", onNav (DeleteClick, comment.commentid), title "Confirm deletion."
                  , class "confirmbutton"
                  ] [ ]
              ]
            else
              div []
              [ a [ href "#", onNav (DeleteClick, comment.commentid), title "Delete this comment."
                  , class "button deletebutton commentbutton"
                  ] [ ]
              , text " "
              , a [ href "#", onNav (EditClick, comment.commentid), title "Edit this comment."
                  , class "button editbutton commentbutton"
                  ] [ ]
              ]
          else
            text ""
        ]
