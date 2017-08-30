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
import Html.Events exposing (onCheck, onInput, onClick, onSubmit, on)
import String exposing (isEmpty, trimLeft)
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline
import Time
import User exposing (..)
import Comment
import GalleryUtils exposing (..)
import Markdown
import Date
import Array
import Char
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
    , uploadPNG : Bool
    , cfdghtml : Html MsgId
    , noteshtml : Html MsgId
    , comments : List Comment.Comment
    , emptyComment : Comment.Comment
    , ready2delete : Bool
    , formPartValid : Array.Array Bool
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

initDesign: String -> String -> Time.Time -> Design
initDesign user ccURI now = 
  let
    (image,name,uri) = 
      if String.contains "/licenses/by/" ccURI then
        ("https://licensebuttons.net/l/by/4.0/88x31.png","Creative Commons Attribution 4.0 International","https://creativecommons.org/licenses/by/4.0/")
      else if String.contains "/licenses/by-sa/" ccURI then
        ("https://licensebuttons.net/l/by-sa/4.0/88x31.png","Creative Commons Attribution-ShareAlike 4.0 International","https://creativecommons.org/licenses/by-sa/4.0/")
      else if String.contains "/licenses/by-nd/" ccURI then
        ("https://licensebuttons.net/l/by-nd/4.0/88x31.png","Creative Commons Attribution-NoDerivatives 4.0 International","https://creativecommons.org/licenses/by-nd/4.0/")
      else if String.contains "/licenses/by-nc/" ccURI then
        ("https://licensebuttons.net/l/by-nc/4.0/88x31.png","Creative Commons Attribution-NonCommercial 4.0 International","https://creativecommons.org/licenses/by-nc/4.0/")
      else if String.contains "/licenses/by-nc-sa/" ccURI then
        ("https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png","Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International","https://creativecommons.org/licenses/by-nc-sa/4.0/")
      else if String.contains "/licenses/by-nc-nd/" ccURI then
        ("https://licensebuttons.net/l/by-nc-nd/4.0/88x31.png","Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International","https://creativecommons.org/licenses/by-nc-nd/4.0/")
      else if String.contains "/publicdomain/zero/" ccURI then
        ("https://licensebuttons.net/p/zero/1.0/88x31.png","CC0 1.0 Universal (CC0 1.0) Public Domain Dedication","https://creativecommons.org/publicdomain/zero/1.0/")
      else
        ("No license chosen", "", "")
  in
    Design image name uri 0 [] "" "" Nothing "" 0 user "" [] "" Untiled "" now "" False
      (text "") (text "") [] (Comment.emptyComment 0) False
      (Array.fromList [False, False, False, True, True, True, True, True, True])

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

validateDesign : String -> String -> Array.Array Bool
validateDesign title var =
  Array.fromList  [ validateTitle title, True, True, True
                  , validateVariation var, True, True, True, True]

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
        |> Json.Decode.Pipeline.required "imagelocation" (Json.Decode.map (String.endsWith ".png") Json.Decode.string)
        |> Json.Decode.Pipeline.hardcoded (text "")
        |> Json.Decode.Pipeline.required "notes" decodeNotesMarkdown
        |> Json.Decode.Pipeline.hardcoded []
        |> Json.Decode.Pipeline.required "designid" (Json.Decode.map Comment.emptyComment Json.Decode.int)
        |> Json.Decode.Pipeline.hardcoded False
        |> Json.Decode.Pipeline.custom  (Json.Decode.map2 
                                          validateDesign 
                                          (Json.Decode.field "title" Json.Decode.string) 
                                          (Json.Decode.field "variation" Json.Decode.string)
                                        )

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
    | CancelDelete
    | AddFavesClick
    | RemoveFavesClick
    | CommentMsg Comment.MsgId
    | CancelEdit
    | TitleChange String
    | FileChange Int
    | VariationChange String

type alias MsgId = (Msg, Int)

commentMsgId : Int -> Comment.MsgId -> MsgId
commentMsgId id cmsgid =
  (CommentMsg cmsgid, id)

update : Msg -> Design -> (Design, Maybe Action)
update msg design =
  case msg of
    DeleteClick -> 
      if design.ready2delete then
        ({design | ready2delete = False}, Just (DeleteDesign design.designid))
      else
        ({design | ready2delete = True}, Nothing)
    CancelDelete -> ({design | ready2delete = False}, Nothing)
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
    CancelEdit -> (design, Just (CancelEditAct))
    TitleChange title_ ->
      ({design | title = title_
               , formPartValid = Array.set 0 (validateTitle title_) design.formPartValid}, Nothing)
    FileChange index ->
      ({design | formPartValid = Array.set index True design.formPartValid}, Nothing)
    VariationChange var_ ->
      ({design | variation = var_
               , formPartValid = Array.set 4 (validateVariation var_) design.formPartValid}, Nothing)



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
        

isAlpha : Char -> Bool
isAlpha char =
  Char.isLower char || Char.isUpper char

validateTitle : String -> Bool
validateTitle title =
  let
    titletrim = String.trim title
  in
    (clamp 3 100 (String.length titletrim)) == (String.length titletrim)

validateVariation : String -> Bool
validateVariation var =
  let
    vartrim = String.trim var
  in
    vartrim == "" || ((String.length vartrim) <= 6 && (String.all isAlpha vartrim))



-- VIEW

validDesign : Design -> Bool
validDesign design =
  Array.foldl (&&) True design.formPartValid

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
    | Edit

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

viewCC : Design -> Html MsgId
viewCC design =
  if isEmpty design.ccURI || isEmpty design.ccName || isEmpty design.ccImage then
    let
      date = Date.fromTime design.uploaddate
    in
        
      text ("Copyright " ++ (toString (Date.year date)) ++ ", all rights reserved.")
  else
    div [class "ccInfo"]
    [ a [class "ccIcon", href design.ccURI]
        [ img [alt "creative commons icon", src design.ccImage][] ]
    , text design.ccName
    ]

downloadLink : String -> Html MsgId -> Html MsgId
downloadLink filepath content =
  let
    mfilename = List.head <| List.reverse <| (String.split "/" filepath)
    filename = Maybe.withDefault filepath mfilename
  in
    a [href filepath, downloadAs filename, title "Download the cfdg file to your computer."]
      [ content ]
      
newTextMsg : Int -> (String -> Msg) -> String -> MsgId
newTextMsg id msg text =
  (msg text, id)

isValid : Int -> Design -> Bool
isValid index design =
  let
    mValid = Array.get index design.formPartValid
  in
    Maybe.withDefault True mValid

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
          , downloadLink design.filelocation 
              ( img [ src "graphics/btn_download.png", alt "Download cfdg",
                      width 126, height 33] []
              )
          , text " "
          , a [ href ("translate.php?id=" ++ toString design.designid),
                title "Translate to new syntax." ] 
              [ img [ src "graphics/btn_translate.png", alt "Translate to new syntax",
                      width 120, height 33 ] []
              ]
          , text " "
          ]
        , if canModify design.owner cfg.currentUser then  
            if design.ready2delete then
              [ a [ href "#", onNav (CancelDelete,design.designid), title "Cancel deletion."] 
                  [ img [ src "graphics/btn_cancel.png", alt "Cancel deletion",
                          width 90, height 33 ][]
                  ]
              , text " "
              , a [ href "#", onNav (DeleteClick,design.designid), title "Confirm deletion."] 
                  [ img [ src "graphics/btn_confirm.png", alt "Confirm deletion",
                          width 112, height 33 ][]
                  ]
              , text " "
              ]
            else
              [ a [ href "#", onNav (DeleteClick,design.designid), title "Delete this design."] 
                  [ img [ src "graphics/btn_delete.png", alt "Delete this design",
                          width 98, height 33 ][]
                  ]
              , text " "
              , a [ href ("#edit/" ++ (toString design.designid)), title "Edit this design."] 
                  [ img [ src "graphics/btn_edit.png", alt "Edit this design",
                          width 80, height 33 ][]
                  ]
              , text " "
              ]
          else
            [ ]
        , case cfg.currentUser of
            Nothing -> [ ]
            Just user ->
              if List.member user.name design.fans then
                [ a [ href "#", onNav (RemoveFavesClick,design.designid), title "Remove this design from your list of favorites."] 
                    [ img [ src "graphics/btn_removefave.png", alt "Remove from favorites",
                            width 112, height 33 ][]
                    ]
                ]
              else
                [ a [ href "#", onNav (AddFavesClick,design.designid), title "Add this design to your list of favorites."] 
                    [ img [ src "graphics/btn_addfave.png", alt "Add to favorites",
                            width 82, height 33 ][]
                    ]
                ]
        , [ br [][]
          , text ("link tag: [link design:" ++ (toString design.designid) ++ "] ... [/link]")
          ]
        , [ viewCC design ]
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
            , downloadLink design.filelocation 
                ( img [ src "graphics/btn_download.png", alt "Download cfdg",
                        width 126, height 33] []
                )
            , text " "
            , a [ href ("#design/" ++ (toString design.designid)), title "View design." ] 
                [ img [ src "graphics/btn_view.png", alt "View Design",
                        width 86, height 33 ] []
                ]
            , text " "
            ]
          , if canModify design.owner cfg.currentUser then
              if design.ready2delete then
                [ a [ href "#", onNav (CancelDelete,design.designid), title "Cancel deletion."] 
                    [ img [ src "graphics/btn_cancel.png", alt "Cancel deletion",
                            width 90, height 33 ][]
                    ]
                , text " "
                , a [ href "#", onNav (DeleteClick,design.designid), title "Confirm deletion."] 
                    [ img [ src "graphics/btn_confirm.png", alt "Confirm deletion",
                            width 112, height 33 ][]
                    ]
                , text " "
                ]
              else
                [ a [ href "#", onNav (DeleteClick,design.designid), title "Delete this design."] 
                    [ img [ src "graphics/btn_delete.png", alt "Delete this design",
                            width 98, height 33 ][]
                    ]
                , text " "
                , a [ href ("#edit/" ++ (toString design.designid)), title "Edit this design."] 
                    [ img [ src "graphics/btn_edit.png", alt "Edit this design",
                            width 80, height 33 ][]
                    ]
                , text " "
                ]
            else
              [ ]
          , [ br [][]
            , text ("link tag: [link design:" ++ (toString design.designid) ++ "] ... [/link]")
            ]
          , [ viewCC design ]
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
              , downloadLink design.filelocation 
                  ( img [ src "graphics/mbtn_download.png", alt "Download cfdg",
                          width 34, height 33] []
                  )
              , text " "
              , a [ href ("#design/" ++ (toString design.designid)), title "View design." ] 
                  [ img [ src "graphics/mbtn_view.png", alt "View Design",
                          width 34, height 33 ] []
                  ]
              , text " "
              ]
            , if canModify design.owner cfg.currentUser then
                if design.ready2delete then
                  [ br [][]
                  , a [ href "#", onNav (CancelDelete,design.designid), title "Cancel deletion."] 
                      [ img [ src "graphics/mbtn_cancel.png", alt "Cancel deletion",
                              width 34, height 33 ][]
                      ]
                  , text " "
                  , a [ href "#", onNav (DeleteClick,design.designid), title "Confirm deletion."] 
                      [ img [ src "graphics/mbtn_confirm.png", alt "Confirm deletion",
                              width 34, height 33 ][]
                      ]
                  , text " "
                  ]
                else
                  [ br [][]
                  , a [ href "#", onNav (DeleteClick,design.designid), title "Delete this design."] 
                      [ img [ src "graphics/mbtn_delete.png", alt "Delete this design",
                              width 34, height 33 ][]
                      ]
                  , text " "
                  , a [ href ("#edit/" ++ (toString design.designid)), title "Edit this design."] 
                      [ img [ src "graphics/mbtn_edit.png", alt "Edit this design",
                              width 34, height 33 ][]
                      ]
                  , text " "
                  ]
              else
                [ ]
            ])
          ]
        ]
    Edit ->
      div []
      [ if design.designid == 0 then
          h1 [] [text "Upload your artwork!"]
        else
          h1 [] [text "Update your artwork!"]
      , div [style [("width", "600px")]]
        [ text "Pick a title for your artwork (e.g. \"People Dancing and Dying\") "
        , text "and upload it here. If you are using "
        , b [] [text "Context Free"]
        , text " for Mac or Windows you can upload directly from the program."
        , br [][]
        , br [][]
        ]
      , Html.form [method "POST", action "http://localhost:5000/postdesign", 
              enctype "multipart/form-data"]
        [ table [class "upload"]
          [ tr []
            [ td [] [b [] [text "Title"], text ":"]
            , td [] [input [type_ "text", size 30, maxlength 100, name "title"
                    , value design.title, onInput (newTextMsg design.designid TitleChange)][]]
            , td 
              [ class "alert"
             , style [("visibility", if (isValid 0 design) then "hidden" else "visible")]
              ] [text "Title must be between 3 and 100 characters."]
            ]
          , tr []
            [ td [] [b [] [text "CFDG"], text " file:"]
            , td [] [ input [type_ "file", name "cfdgfile"
                    , on "change" (Json.Decode.succeed (FileChange 1, design.designid))][]]
            , td 
              [ class "alert"
             , style [("visibility", if (isValid 1 design) then "hidden" else "visible")]
              ] [text "CFDG file must be chosen."]
            ]
          , tr []
            [ td [] [b [] [text "PNG"], text " file:"]
            , td [] [ input [type_ "file", name "imagefile"
                    , on "change" (Json.Decode.succeed (FileChange 2, design.designid))] []]
            , td 
              [ class "alert"
             , style [("visibility", if (isValid 2 design) then "hidden" else "visible")]
              ] [text "PNG file must be chosen."]
            ]
          , tr [] 
            [ td [] [text "Image upload compression type:"]
            , td [] 
              [ input [type_ "radio", name "compression", value "JPEG", checked (not design.uploadPNG)][]
              , text " JPEG "
              , input [type_ "radio", name "compression", value "PNG-8", checked (design.uploadPNG)][]
              , text " PNG-8 "
              ]
            , td [][]
            ]
          , tr []
            [ td [] [b [] [text "Variation"], text ":"]
            , td [] [ input [type_ "text", size 9, maxlength 6, name "variation"
                    , value design.variation, onInput (newTextMsg design.designid VariationChange)][]]
            , td 
             [ class "alert"
             , style [("visibility", if (isValid 4 design) then "hidden" else "visible")]
             ] [text "Variation must be 0 to 6 letters."]
            ]
          , tr []
            [ td [] [b [] [text "Tags"], text ":"]
            , td [] [input [type_ "text", size 30, name "tags", value (String.join " " design.tags)] []]
            , td [][]
            ]
          , tr []
            [ td [] [text "Design is ", b[] [text "tiled"], text " or ", b [][text "frieze"], text ":"]
            , td [] 
              [ select [name "tiledtype", size 1]
                [ option [value "0", selected (design.tiled == Untiled)] [text "Not tiled"]
                , option [value "1", selected (design.tiled == Hfrieze)] [text "Horizontal frieze"]
                , option [value "2", selected (design.tiled == Vfrieze)] [text "Vertical frieze"]
                , option [value "3", selected (design.tiled == Tiled)]   [text "Tiled"]
                ]
              ]
            , td [][]
            ]
          , tr []
            [ td [class "vupload"]
              [ p [] [b [] [text "Author notes"], text ":"]
              , p [] [text "Please, please, please make sure that any included cfdg files are in the gallery as well and put links to them here."]
              ]
            , td [colspan 2]
              [ textarea [rows 5, cols 20, maxlength 1000, name "notes", value design.notes] []
              , div [class "taghelp"]
                [ p [] [text "CFDG code should be put between [code] ... [/code] tags to get formatted properly. "]
                , p [] [text "Link tags should be in one of these three forms:"]
                , ul []
                  [ li [] [text "[link design:505]Decreasing Aperture[/link]"]
                  , li [] [text "[link user:Sykil]other stuff by Sykil[/link]"]
                  , li [] [text "[link http://...]link to some other site[/link]"]
                  ]
                ]
              ]
            ]
          , tr []
            [ td [class "vupload"] [b [] [text "Set License"], text ":"]
            , td [colspan 2]
              [ select [name "cclicense", size 1]
                [ option [value "-", selected True] [text "No change"]
                , optgroup [attribute "label" "Public Domain"]
                  [ option [value "zero"] [text "Creative Commons Zero"]]
                , optgroup [attribute "label" "Creative Commons Licenses"]
                  [ option [value "by"] [text "Creative Commons Attibution"]
                  , option [value "by-sa"] [text "Creative Commons Attibution-ShareAlike"]
                  , option [value "by-nd"] [text "Creative Commons Attibution-NoDerivatives"]
                  , option [value "by-nc"] [text "Creative Commons Attibution-NonCommercial"]
                  , option [value "by-nc-sa"] [text "Creative Commons Attibution-NonCommercial-ShareAlike"]
                  , option [value "by-nc-nd"] [text "Creative Commons Attibution-NonCommercial-NoDerivatives"]
                  ]
                , optgroup [attribute "label" "Default Copyright"]
                  [ option [value ""][text "All Rights Reserved"]]
                ]
              ]
            ]
          , tr []
            [ td [][text "Current License:"]
            , td [colspan 2] [viewCC design]
            ]
          , tr []
            [ td [] 
              [ input 
                [ type_ "submit"
                , value
                    (if design.designid == 0 then
                      "Upload Design"
                    else
                      "Update Design")
                , disabled (not (validDesign design))
                ] []
              , text " "
              , input [type_ "submit", value "Cancel", onNav (CancelEdit,design.designid)] []
              ]
            , td [colspan 2] 
              [ if design.designid == 0 then
                  text ""
                else
                  input [type_ "hidden", name "designid", value (toString design.designid)] []
              ]
            ]
          ]
        ]
      ]
