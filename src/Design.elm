module Design exposing
  ( Design
  , initDesign
  , setCfdg
  , setComments
  , setComment
  , removeComment
  , decodeDesign
  , encodeDesign

  , Msg
  , MsgId
  , update
  , ViewConfig
  , ViewSize (..)
  , view
  )



import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onClick, onSubmit)
import String exposing (isEmpty, trimLeft)
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline
import Time
import User exposing (..)
import Comment
import GalleryUtils exposing (..)
import Markdown
--import Debug

-- MODEL

type alias Size =
    { width : Int
    , height : Int
    }

type TileType = Untiled | Hfrieze | Vfrieze | Tiled

type alias Design =
    { ccImage : String
    , ccName : String
    , ccURI : String
    , designid : Int
    , fans : List String
    , filelocation : String
    , imagelocation : String
    , imagesize : Maybe Size
    , notes : String
    , numvotes : Int
    , owner : String
    , smthumblocation : String
    , tags : List String
    , thumblocation : String
    , tiled : TileType
    , title : String
    , uploaddate : Time.Time
    , variation : String
    , cfdghtml : Html MsgId
    , noteshtml : Html MsgId
    , comments : List Comment.Comment
    , emptyComment : Comment.Comment
    }

options : Markdown.Options
options =
  let
    opts = Markdown.defaultOptions
  in
    { opts | sanitize = True, defaultHighlighting = Just "cfdg" }

toHtml : String -> Html MsgId
toHtml = Markdown.toHtmlWith options []

notesHtml : String -> Html MsgId
notesHtml notes = 
  if isEmpty notes then
    text ""
  else
    Markdown.toHtmlWith options [class "notesdiv"] notes

int2Tiled : Int -> TileType
int2Tiled i =
  case i of
    1 -> Hfrieze
    2 -> Vfrieze
    3 -> Tiled
    _ -> Untiled

tiled2Int : TileType -> Int
tiled2Int tt =
  case tt of
    Untiled -> 0
    Hfrieze -> 1
    Vfrieze -> 2
    Tiled -> 3

initDesign: String -> String -> Design
initDesign user ccURI = 
    Design "" "" ccURI 0 [] "" "" Nothing "" 0 user "" [] "" Untiled "" 0 "" 
      (text "") (text "") [] (Comment.emptyComment 0)

setCfdg : Int -> String -> Design -> Design
setCfdg id newCfdg design =
  if id == design.designid then
    let
      markdown = String.concat ["```cfdg\n", newCfdg, "\n```\n"]
    in
      { design | cfdghtml = toHtml markdown }
  else
    design

    
setComments : List Comment.Comment -> Design -> Design
setComments newComments design =
  { design | comments = List.map Comment.setupHtml newComments }

setComment : Comment.Comment -> Design -> Design
setComment newComment design =
  { design | comments = replaceComment newComment design.comments}

replaceComment : Comment.Comment -> List Comment.Comment -> List Comment.Comment
replaceComment cmt_ cmts =
  case cmts of
    c :: lc -> 
      if c.commentid == cmt_.commentid then
        (Comment.setupHtml cmt_) :: lc
      else
        c :: (replaceComment cmt_ lc)
    [] -> [Comment.setupHtml cmt_]

removeComment : Int -> Design -> Design
removeComment deleteid design =
  let
    comments_ = List.filter (\c -> c.commentid /= deleteid) design.comments
  in
    {design | comments = comments_}      


decodeSize : Json.Decode.Decoder Size
decodeSize = 
  Json.Decode.map2 Size
    (Json.Decode.field "width" Json.Decode.int)
    (Json.Decode.field "height" Json.Decode.int)

decodeNotesMarkdown : Json.Decode.Decoder (Html MsgId)
decodeNotesMarkdown =
  Json.Decode.map notesHtml Json.Decode.string

decodeDesign : Json.Decode.Decoder Design
decodeDesign =
    Json.Decode.Pipeline.decode Design
        |> Json.Decode.Pipeline.required "ccImage" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ccName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ccURI" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "designid" (Json.Decode.int)
        |> Json.Decode.Pipeline.optional "fans" (Json.Decode.list Json.Decode.string) []
        |> Json.Decode.Pipeline.required "filelocation" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "imagelocation" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "imagesize" (Json.Decode.maybe decodeSize) Nothing
        |> Json.Decode.Pipeline.required "notes" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "numvotes" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "owner" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "smthumblocation" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "tags" (Json.Decode.list Json.Decode.string) []
        |> Json.Decode.Pipeline.required "thumblocation" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "tiled" (Json.Decode.map int2Tiled Json.Decode.int)
        |> Json.Decode.Pipeline.required "title" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "uploaddate" (Json.Decode.map int2Time Json.Decode.int)
        |> Json.Decode.Pipeline.required "variation" (Json.Decode.string)
        |> Json.Decode.Pipeline.hardcoded (text "")
        |> Json.Decode.Pipeline.required "notes" decodeNotesMarkdown
        |> Json.Decode.Pipeline.hardcoded []
        |> Json.Decode.Pipeline.required "designid" (Json.Decode.map Comment.emptyComment Json.Decode.int)

encodeDesign : Design -> Json.Encode.Value
encodeDesign record =
    Json.Encode.object
        [ ("ccImage",   Json.Encode.string  <| record.ccImage)
        , ("ccName",    Json.Encode.string  <| record.ccName)
        , ("ccURI",     Json.Encode.string  <| record.ccURI)
        , ("designid",  Json.Encode.int     <| record.designid)
        , ("notes",     Json.Encode.string  <| record.notes)
        , ("tiled",     Json.Encode.int     <| (tiled2Int record.tiled))
        , ("title",     Json.Encode.string  <| record.title)
        , ("variation", Json.Encode.string  <| record.variation)
        ]


-- UPDATE

type Msg
    = DeleteClick
    | EditClick
    | AddFavesClick
    | RemoveFavesClick
    | CommentMsg Comment.MsgId

type alias MsgId = (Msg, Int)

commentMsgId : Int -> Comment.MsgId -> MsgId
commentMsgId id cmsgid =
  (CommentMsg cmsgid, id)

update : Msg -> Design -> (Design, Maybe Action)
update msg design =
  case msg of
    DeleteClick -> (design, Just (DeleteDesign design.designid))
    EditClick -> (design, Just (EditDesign design.designid))
    AddFavesClick -> (design, Just (AddFaves design.designid))
    RemoveFavesClick -> (design, Just (RemoveFaves design.designid))
    CommentMsg (cmsg, id) ->
      if id == 0 then
        let
          (comment_, act) = Comment.update cmsg design.emptyComment
        in
          ({design | emptyComment = comment_}, act)
      else
        let
          (comments_, act) = updateCommentList (cmsg, id) design.comments
        in
          ({design | comments = comments_}, act)



updateCommentList : Comment.MsgId -> List Comment.Comment -> (List Comment.Comment, Maybe Action)
updateCommentList (msg, id) comments =
  case comments of
    c :: lc ->
      if c.commentid == id then
        let
          (c_, act_) = Comment.update msg c
        in
          (c_ :: lc, act_)
      else
        let
          (lc_, act_) = updateCommentList (msg, id) lc
        in
          (c :: lc_, act_)
    [] -> ([], Nothing)
        





-- VIEW

showOnSide : Design -> Bool
showOnSide design =
  case design.imagesize of
    Nothing -> False
    Just sz -> design.tiled == Vfrieze && sz.width <= 150

makeTagLink : String -> Html MsgId
makeTagLink tag = 
  a [href (makeUri "#tag" [tag, "0"])] [text (tag ++ " ")]

makeFanLink : String -> Html MsgId
makeFanLink fan = 
  a [href (makeUri "#user" [fan, "0"])] [b [] [text (fan ++ " ")]]

fanCount : Int -> String
fanCount cnt =
  if cnt == 1 then
    "One vote"
  else
    toString cnt ++ " votes"

tileText : TileType -> String
tileText tile =
  case tile of
    Untiled -> ""
    Hfrieze -> ", horizontal frieze"
    Vfrieze -> ", vertical frieze"
    Tiled -> ", tiled"

type ViewSize
    = Large
    | Medium
    | Small

type alias ViewConfig =
    { size : ViewSize
    , currentUser : Maybe User
    }

fullImageAttributes : Design -> List (Attribute MsgId)
fullImageAttributes design =
  let
    imageurl = "url(" ++ design.imagelocation ++ ")"
  in
    if showOnSide design then
      [ style
        [ ("background-image", imageurl)
        , ("background-repeat", "repeat-y")
        , ("margin-bottom", "5px")
        , ("width", 
            let
              sz = Maybe.withDefault (Size 150 800) design.imagesize
            in
              toString sz.width ++ "px"
          )
        , ("float", "left")
        , ("min-height",
            let
              sz = Maybe.withDefault (Size 150 800) design.imagesize
            in
              toString (2 * sz.height) ++ "px"
          )
        ]
      ]
    else
      case design.tiled of
        Untiled ->
          [ class "fullimagediv"
          ]
        Hfrieze ->
          [ class "tiledimagediv"
          , style 
            [ ("background-image", imageurl)
            , ("background-repeat", "repeat-x")
            , ("height", 
                let
                  sz = Maybe.withDefault (Size 800 800) design.imagesize
                in
                  ((toString sz.height) ++ "px")
              )
            ]
          ]
        Vfrieze ->
          [ class "tiledimagediv"
          , style 
            [ ("background-image", imageurl)
            , ("background-repeat", "repeat-y")
            , ("width", 
                let
                  sz = Maybe.withDefault (Size 800 800) design.imagesize
                in
                  ((toString sz.width) ++ "px")
              )
            ]
          ]
        Tiled ->
          [ class "tiledimagediv"
          , style 
            [ ("background-image", imageurl)
            , ("background-repeat", "repeat")
            ]
          ]

thumbImageAttributes : Design -> List (Attribute MsgId)
thumbImageAttributes design =
  let
    imageurl = "url(" ++ design.imagelocation ++ ")"
    sz = Maybe.withDefault (Size 300 300) design.imagesize
  in
    case design.tiled of
      Untiled ->
        [ class "thumbcell"
        , style 
          [ ("float", "left")
          ]
        ]
      Hfrieze ->
        [ class "thumbcell"
        , style
          [ ("background-image", imageurl)
          , ("background-repeat", "repeat-x")
          , ("height", toString sz.height ++ "px")
          , ("text-align", "left")
          , ("width", "100%")
          ]
        ]
      Vfrieze ->
        [ class "thumbcell"
        , style
          [ ("background-image", imageurl)
          , ("background-repeat", "repeat-y")
          , ("background-position", "right")
          , ("min-height", toString (2 * sz.height) ++ "px")
          , ("float", "left")
          ]
        ]
      Tiled ->
        [ class "thumbcell"
        , style
          [ ("background-image", imageurl)
          , ("background-repeat", "repeat")
          , ("min-height", toString (2 * sz.height) ++ "px")
          , ("float", "left")
          ]
        ]

thumbImage : Design -> Html MsgId
thumbImage design =
  let
    sz = Maybe.withDefault (Size 300 300) design.imagesize
  in
    case design.tiled of
      Untiled ->
        a [ href ("#design/" ++ (toString design.designid))]
          [ img [ class "image", src design.thumblocation, alt "design thumbnail"] []]
      Hfrieze ->
        a [ href ("#design/" ++ (toString design.designid))]
          [ img 
            [ class "image"
            , src "empty300.png"
            , width 300
            , height (sz.height - 1)
            , alt "design thumbnail"
            ] []]
      Vfrieze ->
        a [ href ("#design/" ++ (toString design.designid))]
          [ img 
            [ class "image"
            , src "empty300.png"
            , width (sz.width - 1)
            , height (2 * sz.height - 1)
            , alt "design thumbnail"
            ] []]
      Tiled ->
        a [ href ("#design/" ++ (toString design.designid))]
          [ img 
            [ class "image"
            , src "empty300.png"
            , width 300
            , height 300
            , alt "design thumbnail"
            ] []]


view : ViewConfig -> Design -> Html MsgId
view cfg design =
  case cfg.size of
    Large ->
      div []
      [ div (fullImageAttributes design)
        [ if design.tiled == Untiled then
            img [class "image", src design.imagelocation, alt "cfdg image"] []
          else
            text " "
        ]
      , div 
        ( if showOnSide design then
            [ style [("padding-left", "150px")]
            ]
          else
            []
        )
        (List.concat
        [ [ b [] [ text design.title ]
          , br [] []
          , text "by " 
          , a [href (makeUri "#user" [design.owner, "0"])] 
              [ b [] [text design.owner]]
          ]
        , if isEmpty design.variation then
            [ text "" ]
          else
            [ text ", Variation: ", b [] [text design.variation] ]
        , [ b [] [text (tileText design.tiled)]
          , text (", uploaded on " ++ (makeDate design.uploaddate))
          ]
        , if not (List.isEmpty design.tags) then
            [ div [class "pte_tags_form"] 
               ([text "Tags: "] ++ (List.map makeTagLink design.tags))
            ]
          else
            []
        , if not (List.isEmpty design.fans) then
            [ div [id "favelist"] 
               ([text (fanCount design.numvotes), text ": "] ++ 
                (List.map makeFanLink design.fans))
            ]
          else
            []
        , [ br [] []
          , a [ href design.filelocation, download True, 
                title "Download the cfdg file to your computer." ]
              [ img [ src "graphics/downloadButton.png", alt "Download cfdg",
                      width 100, height 22] []
              ]
          , text " "
          , a [ href ("translate.php?id=" ++ toString design.designid),
                title "Translate to new syntax." ] 
              [ img [ src "graphics/translateButton.png", alt "Translate to new syntax",
                      width 83, height 22 ] []
              ]
          , text " "
          ]
        , if canModify design.owner cfg.currentUser then  
            [ a [ href "#", onClick (DeleteClick,design.designid), title "Delete this design."] 
                [ img [ src "graphics/deleteButton.png", alt "Delete this design",
                        width 80, height 22 ][]
                ]
            , text " "
            , a [ href "#", onClick (EditClick,design.designid), title "Edit this design."] 
                [ img [ src "graphics/editButton.png", alt "Edit this design",
                        width 60, height 22 ][]
                ]
            , text " "
            ]
          else
            [ ]
        , case cfg.currentUser of
            Nothing -> [ ]
            Just user ->
              if List.member user.name design.fans then
                [ a [ href "#", onClick (RemoveFavesClick,design.designid), title "Remove this design from your list of favorites."] 
                    [ img [ src "graphics/deleteFaveButton.png", alt "Remove from favorites",
                            width 90, height 22 ][]
                    ]
                ]
              else
                [ a [ href "#", onClick (AddFavesClick,design.designid), title "Add this design to your list of favorites."] 
                    [ img [ src "graphics/addFaveButton.png", alt "Add to favorites",
                            width 65, height 22 ][]
                    ]
                ]
        , [ br [][]
          , text ("link tag: [link design:" ++ (toString design.designid) ++ "] ... [/link]")
          ]
        , if isEmpty design.ccURI || isEmpty design.ccName || isEmpty design.ccImage then
            []
          else
            [ div [class "ccInfo"]
              [ a [class "ccIcon", href design.ccURI]
                  [ img [alt "creative commons icon", src design.ccImage][] ]
              , text design.ccName
              ]
            ]
        , [ br [] [] 
          , table [style [("table-layout","fixed"),("width","100%")]]
            [ tr []
              [ td [class "halfcell"]
                [ div [class "filediv"]
                  [ design.noteshtml
                  , design.cfdghtml
                  ]
                , div [class "disclaimer"]
                  [ br [] []
                  , br [] []
                  , text """
    An uploaded image and the corresponding CFDG are both presumed to be owned by the uploader. 
    The CFDG Gallery makes no claims about ownership whatsoever. Peace!
    """
                  ]
                ]
              , td [class "commentcell"]
                ( let
                    editing = List.any Comment.isEditable design.comments
                    user = if editing then Nothing else cfg.currentUser
                  in
                    [ div [class "commentsdiv"]
                      (List.intersperse (hr [][])
                        (List.map ((Comment.view user) >> (Html.map (commentMsgId design.designid)))
                          ( design.comments
                            ++
                            if cfg.currentUser == Nothing || editing then
                              []
                            else
                              [design.emptyComment]
                          )
                        )
                      )
                    ]
                )
              ]
            ]
          ]
        ])
      ]
    Medium ->
      div [style [("overflow", "auto")]]
      [ div (thumbImageAttributes design)
        [ thumbImage design ]
      , div [style [("padding-left", "310px")]]
          (List.concat
          [ [ b [] [text design.title ]
            , text " by "
            , a [ href (makeUri "#user" [design.owner, "0"]) ] 
                [ b [] [text design.owner] ]
            ]
            , if isEmpty design.variation then
                [ text "" ]
              else
                [ text ", Variation: ", b [] [text design.variation] ]
            , [ b [] [text (tileText design.tiled)]
              , text (", uploaded on " ++ (makeDate design.uploaddate))
              ]
            , if design.numvotes > 0 then
                [ br [] []
                , span [class "small"] [text (fanCount design.numvotes) ]
                ]
              else
                []
          , [ br [] []
            , a [ href design.filelocation, download True, 
                  title "Download the cfdg file to your computer." ]
                [ img [ src "graphics/downloadButton.png", alt "Download cfdg",
                        width 100, height 22] []
                ]
            , text " "
            , a [ href ("#design/" ++ (toString design.designid)), title "View design." ] 
                [ img [ src "graphics/viewButton.png", alt "View Design",
                        width 70, height 22 ] []
                ]
            , text " "
            ]
          , if canModify design.owner cfg.currentUser then  
              [ a [ href "#", onClick (DeleteClick,design.designid), title "Delete this design."] 
                  [ img [ src "graphics/deleteButton.png", alt "Delete this design",
                          width 80, height 22 ][]
                  ]
              , text " "
              , a [ href "#", onClick (EditClick,design.designid), title "Edit this design."] 
                  [ img [ src "graphics/editButton.png", alt "Edit this design",
                          width 60, height 22 ][]
                  ]
              , text " "
              ]
            else
              [ ]
          , [ br [][]
            , text ("link tag: [link design:" ++ (toString design.designid) ++ "] ... [/link]")
            ]
          , if isEmpty design.ccURI || isEmpty design.ccName || isEmpty design.ccImage then
              []
            else
              [ div [class "ccInfo"]
                [ a [class "ccIcon", href design.ccURI]
                    [ img [alt "creative commons icon", src design.ccImage][] ]
                , text design.ccName
                ]
              ]
          , [ br [] []
            , div [class "filediv", style [("width","95%")]]
                [ design.noteshtml
                , design.cfdghtml
                ]
            ]
          ])
      ]
    Small ->
      table [class "sm_thumbtable"]
        [ tr []
          [ td [class "sm_thumbcell"]
            [ a [ href ("#design/" ++ (toString design.designid)) ]
              [ img [ class "image", src design.smthumblocation, alt "design thumbnail"] []]
            ]
          , td []
            (List.concat
            [ [ b [] [text design.title ]
              , br [] []
              , text " by "
              , a [ href (makeUri "#user" [design.owner, "0"]) ] 
                  [ b [] [text design.owner] ]
              ]
              , if design.numvotes > 0 then
                  [ br [] []
                  , span [class "small"] [text (fanCount design.numvotes) ]
                  ]
                else
                  []
            , [ br [][]
              , a [ href design.filelocation, download True, 
                    title "Download the cfdg file to your computer." ]
                  [ img [ src "graphics/downloadMiniButton.png", alt "Download cfdg",
                          width 30, height 22] []
                  ]
              , text " "
              , a [ href ("#design/" ++ (toString design.designid)), title "View design." ] 
                  [ img [ src "graphics/viewMiniButton.png", alt "View Design",
                          width 30, height 22 ] []
                  ]
              , text " "
              ]
            , if canModify design.owner cfg.currentUser then  
                [ br [][]
                , a [ href "#", onClick (DeleteClick,design.designid), title "Delete this design."] 
                    [ img [ src "graphics/deleteMiniButton.png", alt "Delete this design",
                            width 30, height 22 ][]
                    ]
                , text " "
                , a [ href "#", onClick (EditClick,design.designid), title "Edit this design."] 
                    [ img [ src "graphics/editMiniButton.png", alt "Edit this design",
                            width 30, height 22 ][]
                    ]
                , text " "
                ]
              else
                [ ]
            ])
          ]
        ]
