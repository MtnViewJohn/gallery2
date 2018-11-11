module Design exposing
  ( EditDesign
  , DisplayDesign
  , Design
  , initDesign
  , setCfdg
  , setFile
  , setComments
  , setComment
  , removeComment
  , decodeDDesign
  , decodeEDesign
  , decodeSize
  , encodeDesign
  , makeEDesign
  , makeDDesign

  , Msg
  , MsgId
  , EMsg
  , update
  , editupdate
  , ViewConfig
  , ViewSize (..)
  , Size
  , view
  , viewEdit
  , viewEditTags
  )



import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onClick, onSubmit, on, targetValue, onFocus, onBlur)
import String exposing (isEmpty, trimLeft)
import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JPipe
import Time
import User exposing (..)
import Comment
import GalleryUtils exposing (..)
import Markdown
import Date
import Char
import Ports exposing (FilePortData, fileSelected, fileContentRead)

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
    , designid : DesignID
    , fans : List String
    , filelocation : String
    , imagelocation : String
    , imagesize : Maybe Size
    , thumbsize : Maybe Size
    , notes : String
    , notesmd : String
    , numvotes : Int
    , owner : String
    , smthumblocation : String
    , tags : List String
    , thumblocation : String
    , tiled : TileType
    , title : String
    , uploaddate : Time.Time
    , variation : String
    }

type alias EditDesign = 
    { design : Design
    , ccLicense : String
    , newTag : String
    , filePortData : Maybe FilePortData
    , imagePortData : Maybe FilePortData
    , fileSelected : Bool
    , imageSelected : Bool
    , uploadPNG : Bool
    , focusTag : Bool
    , focusTaglist : Bool
    , tagSelected : Bool
    }

makeEDesign : Design -> EditDesign
makeEDesign design =
  EditDesign design "-" "" Nothing Nothing False False (String.endsWith ".png" design.imagelocation) 
    False False False

type alias DisplayDesign =
    { design : Design
    , cfdghtml : Html MsgId
    , noteshtml : Html MsgId
    , comments : List Comment.Comment
    , emptyComment : Comment.Comment
    }

makeDDesign : Design -> DisplayDesign
makeDDesign design =
  DisplayDesign design (text "") (notesHtml design.notesmd) [] 
    (Comment.emptyComment)


options : Markdown.Options
options =
  let
    opts = Markdown.defaultOptions
  in
    { opts | githubFlavored = Just { tables = False, breaks = True }
           , sanitize = True
           , defaultHighlighting = Just "cfdg" }

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

str2Tiled : String -> TileType
str2Tiled i =
  case i of
    "1" -> Hfrieze
    "2" -> Vfrieze
    "3" -> Tiled
    _   -> Untiled

tiled2Int : TileType -> Int
tiled2Int tt =
  case tt of
    Untiled -> 0
    Hfrieze -> 1
    Vfrieze -> 2
    Tiled -> 3

initDesign: String -> String -> Time.Time -> EditDesign
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
    design = Design image name uri (ID 0) [] "" "" Nothing Nothing "" "" 0 user "" [] "" Untiled "" now ""
  in
    makeEDesign design

setCfdg : String -> DisplayDesign -> DisplayDesign
setCfdg newCfdg ddesign =
  let
    markdown = String.concat ["```cfdg\n", newCfdg, "\n```\n"]
  in
    { ddesign | cfdghtml = toHtml markdown }

setFile : FilePortData -> EditDesign -> EditDesign
setFile fpd edesign =
  if fpd.fileid == "cfdgfile" then
    {edesign | filePortData = Just fpd, fileSelected = True}
  else
    {edesign | imagePortData = Just fpd, imageSelected = True}
    
setComments : List Comment.Comment -> DisplayDesign -> DisplayDesign
setComments newComments ddesign =
  { ddesign | comments = List.map Comment.setupHtml newComments }

setComment : Comment.Comment -> DisplayDesign -> DisplayDesign
setComment newComment ddesign =
  { ddesign | comments = replaceComment newComment ddesign.comments}

replaceComment : Comment.Comment -> List Comment.Comment -> List Comment.Comment
replaceComment cmt_ cmts =
  case cmts of
    c :: lc -> 
      if c.commentid == cmt_.commentid then
        (Comment.setupHtml cmt_) :: lc    -- update existing comment
      else
        c :: (replaceComment cmt_ lc)
    [] -> [Comment.setupHtml cmt_]        -- append new comment

removeComment : CommentID -> DisplayDesign -> DisplayDesign
removeComment deleteid ddesign =
  let
    comments_ = List.filter (\c -> c.commentid /= deleteid) ddesign.comments
  in
    {ddesign | comments = comments_}      

decodeSize : JD.Decoder Size
decodeSize = 
  JD.map2 Size
    (JD.field "width" JD.int)
    (JD.field "height" JD.int)

decodeNotesMarkdown : JD.Decoder (Html MsgId)
decodeNotesMarkdown =
  JD.map notesHtml JD.string


decodeDesign : JD.Decoder Design
decodeDesign =
    JPipe.decode Design
        |> JPipe.required "ccImage" (JD.string)
        |> JPipe.required "ccName" (JD.string)
        |> JPipe.required "ccURI" (JD.string)
        |> JPipe.required "designid" (JD.map ID JD.int)
        |> JPipe.optional "fans" (JD.list JD.string) []
        |> JPipe.required "filelocation" (JD.string)
        |> JPipe.required "imagelocation" (JD.string)
        |> JPipe.optional "imagesize" (JD.maybe decodeSize) Nothing
        |> JPipe.optional "thumbsize" (JD.maybe decodeSize) Nothing
        |> JPipe.required "notes" (JD.string)
        |> JPipe.required "notesmd" (JD.string)
        |> JPipe.required "numvotes" (JD.int)
        |> JPipe.required "owner" (JD.string)
        |> JPipe.required "smthumblocation" (JD.string)
        |> JPipe.optional "tags" (JD.list JD.string) []
        |> JPipe.required "thumblocation" (JD.string)
        |> JPipe.required "tiled" (JD.map int2Tiled JD.int)
        |> JPipe.required "title" (JD.string)
        |> JPipe.required "uploaddate" (JD.map int2Time JD.int)
        |> JPipe.required "variation" (JD.string)

decodeDDesign : JD.Decoder DisplayDesign
decodeDDesign =
  JD.map makeDDesign decodeDesign

decodeEDesign : JD.Decoder EditDesign
decodeEDesign =
  JD.map makeEDesign decodeDesign

encodeDesign : EditDesign -> JE.Value
encodeDesign record =
  case record.design.designid of
    ID designid ->
      JE.object
          [ ("ccImage",    JE.string  <| record.design.ccImage)
          , ("ccName",     JE.string  <| record.design.ccName)
          , ("ccURI",      JE.string  <| record.design.ccURI)
          , ("cclicense",  JE.string  <| record.ccLicense)
          , ("designid",   JE.int     <| designid)
          , ("tags",       JE.list    <| List.map JE.string record.design.tags)
          , ("notes",      JE.string  <| record.design.notes)
          , ("tiled",      JE.int     <| (tiled2Int record.design.tiled))
          , ("title",      JE.string  <| record.design.title)
          , ("variation",  JE.string  <| record.design.variation)
          , ("compression",JE.string  <| if record.uploadPNG then "PNG-8" else "JPEG")
          , ("cfdgfile",   (encodeMaybe encodeFileData) <| record.filePortData)
          , ("imagefile",  (encodeMaybe encodeFileData) <| record.imagePortData)
          ]

encodeFileData : FilePortData -> JE.Value
encodeFileData fpd =
    JE.object
        [ ("filename", JE.string <| fpd.filename)
        , ("contents", JE.string <| stripUrl <| fpd.contents)
        ]

encodeMaybe : (a -> JE.Value) -> (Maybe a) -> JE.Value
encodeMaybe valEncode mVal =
  case mVal of
    Nothing -> JE.null
    Just val -> valEncode val

stripUrl : String -> String
stripUrl url =
  let
    indices = String.indices ";base64," url
    first = List.head indices
  in case first of
    Nothing -> url
    Just index -> String.dropLeft (index + 8) url



realTags : List String -> List String
realTags = List.filter (\t -> t /= "")


-- UPDATE

type Msg
    = DeleteClick
    | CancelDelete
    | FocusClick
    | DismissDesign
    | ViewFans Bool
    | AddFavesClick
    | RemoveFavesClick
    | CommentMsg Comment.MsgId


type EMsg
    = CancelEdit
    | TitleChange String
    | FileChange String
    | VariationChange String
    | CCchange String
    | TiledChange String
    | PNGchoose Bool
    | TagType String
    | TagDelete String
    | TagAdd
    | TagFocus Bool
    | TagsFocus Bool
    | TagSelect String
    | NotesChange String
    | Upload
    | UploadTags

type alias MsgId = (Msg, DesignID)

commentMsgId : DesignID -> Comment.MsgId -> MsgId
commentMsgId id cmsgid =
  (CommentMsg cmsgid, id)

update : Msg -> DisplayDesign -> (DisplayDesign, Maybe Action)
update msg ddesign =
  case msg of
    DeleteClick -> (ddesign, Just (DeleteDesign ddesign.design.designid))
    CancelDelete -> (ddesign, Just (DeleteDesign nonDesign))
    FocusClick -> (ddesign, Just (Focus ddesign.design.designid))
    DismissDesign -> (ddesign, Just CloseDesign)
    ViewFans v -> (ddesign, Just <| ShowFans v)
    AddFavesClick -> (ddesign, Just (AddFaves ddesign.design.designid))
    RemoveFavesClick -> (ddesign, Just (RemoveFaves ddesign.design.designid))
    CommentMsg (cmsg, id) ->
      if id == noComment then
        let
          (comment_, act) = Comment.update cmsg ddesign.emptyComment
        in
          ({ddesign | emptyComment = comment_}, act)
      else
        let
          (comments_, act) = updateCommentList (cmsg, id) ddesign.comments
        in
          ({ddesign | comments = comments_}, act)

editupdate : EMsg -> EditDesign -> (EditDesign, Maybe Action)
editupdate msg edesign = case msg of
    CancelEdit -> (edesign, Just CancelEditAct)
    TitleChange title_ ->
      let
        design = edesign.design
        design_ = {design | title = title_}
      in
        ({edesign | design = design_}, Nothing)
    FileChange id ->
      (edesign , Just (GetFile id))
    VariationChange var_ ->
      let
        design = edesign.design
        design_ = {design | variation = var_}
      in
        ({edesign | design = design_}, Nothing)
    CCchange license_ ->
      ({edesign | ccLicense = license_}, Nothing)
    TiledChange tiled_ ->
      let
        design = edesign.design
        design_ = {design | tiled = str2Tiled tiled_}
      in
        ({edesign | design = design_}, Nothing)
    PNGchoose png_ ->
      ({edesign | uploadPNG = png_}, Nothing)
    TagType tags_ ->
      ({edesign | newTag = String.filter isGraph tags_, tagSelected = False}, Nothing)
    TagDelete tag ->
      let
        design = edesign.design
        tags_ = List.filter (\x -> x /= tag) design.tags
        design_ = {design | tags = tags_}
      in
        ({edesign | design = design_}, Nothing)
    TagAdd ->
      let
        design = edesign.design
        tags_ =  design.tags ++ [edesign.newTag]
        design_ = {design | tags = tags_}
      in
        if (List.member edesign.newTag design.tags) || (String.isEmpty edesign.newTag) then
          ({edesign | newTag = ""}, Nothing)
        else
          ({edesign | design = design_, newTag = ""}, Nothing)
    TagFocus focus ->
      ({edesign | focusTag = focus}, Nothing)
    TagsFocus focus ->
      ({edesign | focusTaglist = focus}, Nothing)
    TagSelect tag ->
      if tag /= " " then
        ({edesign | newTag = tag, tagSelected = True}, Nothing)
      else
        (edesign, Nothing)
    NotesChange notes_ ->
      let
        design = edesign.design
        design_ = {design | notes = notes_}
      in
        ({edesign | design = design_}, Nothing)
    Upload ->
      (edesign, Just UploadDesign)
    UploadTags ->
      (edesign, Just UploadDesignTags)



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

isGraph : Char -> Bool
isGraph char =
  case char of
    ' ' -> False
    '\t' -> False
    '\n' -> False
    '\r' -> False
    '\f' -> False
    '\v' -> False
    _ -> True

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


validateCfdg : EditDesign -> Bool
validateCfdg edesign = case edesign.filePortData of
  Nothing -> not (String.isEmpty edesign.design.filelocation)
  Just fpd -> String.endsWith ".cfdg" (String.toLower fpd.filename)

validateImage : EditDesign -> Bool
validateImage edesign = case edesign.imagePortData of
  Nothing -> not (String.isEmpty edesign.design.imagelocation)
  Just fpd -> String.endsWith ".png" (String.toLower fpd.filename)


-- VIEW

validDesign : EditDesign -> Bool
validDesign edesign =
  (validateTitle edesign.design.title) && 
  (validateVariation edesign.design.variation) &&
  (validateCfdg edesign) && (validateImage edesign)

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
  a [href (makeUri "#faves" [fan, "0"])] [b [] [text fan]]

fanCount : Int -> String
fanCount cnt =
  if cnt == 1 then
    "One like"
  else
    String.fromInt cnt ++ " likes"

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
    | Mini

type alias ViewConfig =
    { size : ViewSize
    , currentUser : Maybe User
    , focus : DesignID
    , prev : DesignID
    , next : DesignID
    , readyToDelete : DesignID
    , commentToDelete : CommentID
    , miniListMode : Bool
    , showFanlist : Bool
    }

fullImageAttributes : Design -> List (Attribute MsgId)
fullImageAttributes design =
  let
    imageurl = "url(" ++ design.imagelocation ++ ")"
  in
    if showOnSide design then
      [ style "background-image" imageurl
      , style "background-repeat" "repeat-y"
      , style "margin-bottom" "5px"
      , style "width" 
          let
            sz = Maybe.withDefault (Size 150 800) design.imagesize
          in
            String.fromInt sz.width ++ "px"
      , style "float" "left"
      , style "min-height"
          let
            sz = Maybe.withDefault (Size 150 800) design.imagesize
          in
            String.fromInt (2 * sz.height) ++ "px"
      , style "position" "absolute"
      , style "height" "100%"
      , style "margin-top" "-36px"
      ]
    else
      case design.tiled of
        Untiled ->
          [ class "fullimagediv"
          ]
        Hfrieze ->
          [ class "tiledimagediv"
          , style "background-image" imageurl
          , style "background-repeat" "repeat-x"
          , style "height"
              let
                sz = Maybe.withDefault (Size 800 800) design.imagesize
              in
                ((String.fromInt sz.height) ++ "px")
          ]
        Vfrieze ->
          [ class "tiledimagediv"
          , style "background-image" imageurl
          , style "background-repeat" "repeat-y"
          , style "width"
              let
                sz = Maybe.withDefault (Size 800 800) design.imagesize
              in
                ((String.fromInt sz.width) ++ "px")
          ]
        Tiled ->
          [ class "tiledimagediv"
          , style "background-image" imageurl
          , style "background-repeat" "repeat"
          ]

thumbImageAttributes : Design -> List (Attribute MsgId)
thumbImageAttributes design =
  let
    imageurl = "url(" ++ design.thumblocation ++ ")"
    sz = Maybe.withDefault (Size 300 300) design.thumbsize
  in
    case design.tiled of
      Untiled ->
        [ class "thumbcell"
        , style "float" "left"
        ]
      Hfrieze ->
        [ class "thumbcell"
        , style "background-image" imageurl
        , style "background-repeat" "repeat-x"
        , style "text-align" "left"
        , style "width" "100%"
        ]
      Vfrieze ->
        [ class "thumbcell"
        , style "background-image" imageurl
        , style "background-repeat" "repeat-y"
        , style "background-position" "right"
        , style "height" "100%"
        , style "position" "absolute"
        , style "float" "left"
        ]
      Tiled ->
        [ class "thumbcell"
        , style "background-image" imageurl
        , style "background-repeat" "repeat"
        , style "height" "100%"
        , style "position" "absolute"
        , style "float" "left"
        ]

thumbImage : Design -> Html MsgId
thumbImage design =
  let
    sz = Maybe.withDefault (Size 300 300) design.thumbsize
  in
    case design.tiled of
      Untiled ->
        a [ href <| "#design/" ++ (idStr design.designid), onNav (FocusClick, design.designid)]
          [ img [ class "image", src design.thumblocation, alt "design thumbnail"] []]
      Hfrieze ->
        a [ href <| "#design/" ++ (idStr design.designid), onNav (FocusClick, design.designid)]
          [ img 
            [ class "image"
            , src "empty300.png"
            , width 300
            , height (sz.height - 1)
            , alt "design thumbnail"
            ] []]
      Vfrieze ->
        a [ href <| "#design/" ++ (idStr design.designid), onNav (FocusClick, design.designid)]
          [ img 
            [ class "image"
            , src "empty300.png"
            , width (sz.width - 1)
            , height (2 * sz.height - 1)
            , alt "design thumbnail"
            ] []]
      Tiled ->
        a [ href <| "#design/" ++ (idStr design.designid), onNav (FocusClick, design.designid)]
          [ img 
            [ class "image"
            , src "empty300.png"
            , width 300
            , height 300
            , alt "design thumbnail"
            ] []]

minHeight : Design -> Int
minHeight design =
  let
    sz = Maybe.withDefault (Size 300 300) design.thumbsize
  in
    case design.tiled of
      Untiled -> sz.height + 5
      Hfrieze -> sz.height + 5
      Vfrieze -> 2 * sz.height + 5
      Tiled   -> 2 * sz.height + 5


viewCC : Design -> Html msg
viewCC design =
  if isEmpty design.ccURI || isEmpty design.ccName || isEmpty design.ccImage then
    let
      date = Date.fromTime design.uploaddate
    in
        
      text ("Copyright " ++ (String.fromInt (Date.year date)) ++ ", all rights reserved.")
  else
    div [class "ccInfo"]
    [ a [class "ccIcon", href design.ccURI]
        [ img [alt "creative commons icon", src design.ccImage][] ]
    , text design.ccName
    ]

downloadLink : String -> String -> Html MsgId
downloadLink filepath content =
  let
    mfilename = List.head <| List.reverse <| (String.split "/" filepath)
    filename = Maybe.withDefault filepath mfilename
  in
    a [href filepath, downloadAs filename, title "Download the cfdg file to your computer."
      , class "button download"
      ] [ text content ]
      
imageLink : String -> String -> Html MsgId
imageLink filepath content =
  let
    mfilename = List.head <| List.reverse <| (String.split "/" filepath)
    filename = Maybe.withDefault filepath mfilename
    mfilename2 = List.head <| String.split "?" filename
    filename2 = Maybe.withDefault filename mfilename2
  in
    a [href filepath, downloadAs filename2, title "Download the image to your computer."
      , class "button download"
      ] [ text content ]
      
makeSelectAttrs : String -> String -> List (Html.Attribute EMsg)
makeSelectAttrs val state =
  [value val, selected (val == state)]

onSelect : (String -> EMsg) -> Attribute EMsg
onSelect tagger =
  on "change" (JD.map tagger targetValue)

thumbLink : ViewConfig -> String -> DesignID -> List (Html MsgId) -> Html MsgId
thumbLink cfg linkClass id contents =
  if cfg.miniListMode then
    a [ href <| "#design/" ++ (idStr id)
      , title "View design."
      , class linkClass 
      ] contents
  else
    a [ href <| "#design/" ++ (idStr id)
      , onNav (FocusClick,id)
      , title "View design."
      , class linkClass 
      ] contents

viewEditDeleteButtons : Bool -> ViewConfig -> DisplayDesign -> List (Html MsgId)
viewEditDeleteButtons icons cfg design =
  if canModify design.design.owner cfg.currentUser then  
    if cfg.readyToDelete == design.design.designid then
      [ a [ href "#", onNav (CancelDelete,design.design.designid), title "Cancel deletion."
          , class "keepbutton"
          ] [ text <| if icons then "" else "Cancel"]
      , text " "
      , a [ href "#", onNav (DeleteClick,design.design.designid), title "Confirm deletion."
          , class "confirmbutton"
          ] [ text <| if icons then "" else "Confirm"]
      , text " "
      ]
    else
      [ a [ href "#", onNav (DeleteClick,design.design.designid), title "Delete this design."
          , class "button deletebutton" 
          ] [ text  <| if icons then "" else " Delete "]
      , text " "
      , a [ href ("#edit/" ++ (idStr design.design.designid)), title "Edit this design."
          , class "button editbutton"
          ] [ text  <| if icons then "" else " Edit "]
      , text " "
      ]
  else
    if not icons && canTag design.design.owner cfg.currentUser then
      [ a [ href ("#edittags/" ++ (idStr design.design.designid)), title "Edit this design's tags."
          , class "button editbutton"
          ] [ text "Edit Tag"]
      ]
    else
      [ ]



viewDesignInfo : ViewSize -> ViewConfig -> DisplayDesign -> List (Html MsgId)
viewDesignInfo size cfg design =
  case size of
    Large ->
      [ b [] [ text design.design.title ]
      , br [] []
      , text "by " 
      , a [href (makeUri "#user" [design.design.owner, "0"])] 
          [ b [] [text design.design.owner]]
      , if isEmpty design.design.variation then
          text ""
        else
          span [] [text ", Variation: ", b [] [text design.design.variation]]
      , b [] [text (tileText design.design.tiled)]
        , text (", uploaded on " ++ (makeDate design.design.uploaddate))
      , if not (List.isEmpty design.design.tags) then
          div [] 
           ([text "Tags: "] ++ (List.map makeTagLink design.design.tags))
        else
          text " "
      , div [class "buttondiv"] 
        ( [ downloadLink design.design.filelocation " Download CFDG "
          , text " "
          , imageLink design.design.imagelocation " Download Image "
          , text " "
          , a [href (makeUri "#translate" [idStr design.design.designid]), title "Translate v2 to v3"
              , class "button translate"
              ] [text "Translate"]
          , text " "
          ] ++
          ( viewEditDeleteButtons False cfg design
          ) ++
          (
            [ div [id "favelist"] 
              (( case cfg.currentUser of
                  Nothing -> text ""
                  Just user ->
                    if List.member user.name design.design.fans then
                      a [ href "#", onNav (RemoveFavesClick,design.design.designid)
                        , title "Click to 'Unlike' this design."
                        , class "button favebutton removefave"
                        ] [ text ""]
                    else
                      a [ href "#", onNav (AddFavesClick,design.design.designid)
                        , title "Click to 'Like' this design."
                        , class "button favebutton addfave"
                        ] [ text ""]
              ) ::
              ( if not (List.isEmpty design.design.fans) then
                  [ text (fanCount design.design.numvotes), text ": "
                  , a [href "#", onNav ((ViewFans True),design.design.designid)] 
                      [text "See who liked it"]
                  , div
                    [ class "popup"
                    , style "display" if cfg.showFanlist then "block" else "none"
                    ]
                  ( [ h2 [] [text "Fans:"]
                    , a [class "close", href "#", onNav ((ViewFans False),design.design.designid)]
                        [text "✖"]
                    ] ++ 
                    (List.intersperse (text ", ") <| List.map makeFanLink design.design.fans)
                  )
                  ] 
                else
                  []
              ))
            ]
          )
        )
      ]
    Medium ->
      [ b [] [text design.design.title ]
      , text " by "
      , a [ href (makeUri "#user" [design.design.owner, "0"]) ] 
          [ b [] [text design.design.owner] ]
      , if isEmpty design.design.variation then
          text ""
        else
          span [] [text ", Variation: ", b [] [text design.design.variation]]
      , b [] [text (tileText design.design.tiled)]
      , text (", uploaded on " ++ (makeDate design.design.uploaddate))
      , if design.design.numvotes > 0 then
          div [class "small"] [text (fanCount design.design.numvotes) ]
        else
          div [] []
      , div [class "buttondiv"]
        ( [ downloadLink design.design.filelocation " Download CFDG "
          , text " "
          , a [ href <| "#design/" ++ (idStr design.design.designid), onNav (FocusClick,design.design.designid), title "View design."
              , class "button viewbutton" 
              ] [ text " View "]
          , text " "
          ] ++
          viewEditDeleteButtons False cfg design
        )
      , div [] [text ("link tag: [link design:" ++ (idStr design.design.designid) ++ "] ... [/link] ")]
      , div [style "padding-bottom" "10px"] [ viewCC design.design ]
      ]
    _ ->
      [ b [] [text design.design.title ]
      , br [] []
      , text " by "
      , a [ href (makeUri "#user" [design.design.owner, "0"]) ] 
          [ b [] [text design.design.owner] ]
      , if design.design.numvotes > 0 then
          div [class "small"] [text (fanCount design.design.numvotes)]
        else
          text " "
      , div []
        (
          [ downloadLink design.design.filelocation ""
          , text " "
          , thumbLink cfg "button viewbutton" design.design.designid []
          , text " "
          ] ++ 
          viewEditDeleteButtons True cfg design
        )
      ]

view : ViewConfig -> DisplayDesign -> Html MsgId
view cfg design =
  let
    size = if cfg.focus == design.design.designid then Large else cfg.size
    addHRs = size == Large && (cfg.size == Small || cfg.size == Mini)
  in case size of
    Large ->
      div 
      [ id ("design" ++ (idStr design.design.designid))
      , style "position" "relative"
      ]
      [ if addHRs then
          hr [] []
        else
          text ""
          , div [class "khomut"]
            [ a 
              [ style "visibility" if cfg.prev == nonDesign then "hidden" else "visible"
              , class "pcnbutton prevbutton"
              , href <| "#design/" ++ (idStr cfg.prev)
              , onNav (FocusClick,cfg.prev)
              , title "Previous design."
              ] []
            , text " "
            , a 
              [ style "visibility"
                      if cfg.prev == nonDesign && cfg.next == nonDesign then 
                        "hidden" 
                      else
                        "visible"
              , class "pcnbutton closebutton"
              , href "#", onNav (DismissDesign,design.design.designid)
              , title "Close this design"
              ] []
            , text " "
            , a 
              [ style "visibility" if cfg.next == nonDesign then "hidden" else "visible"
              , class "pcnbutton nextbutton"
              , href <| "#design/" ++ (idStr cfg.next)
              , onNav (FocusClick,cfg.next)
              , title "Next design."
              ] []
            ]
      , div (fullImageAttributes design.design)
        [ if design.design.tiled == Untiled then
            img [class "image", src design.design.imagelocation, alt "cfdg image"] []
          else
            text " "
        ]
      , div 
        ( if showOnSide design.design then
            [ style "padding-left" "150px" ]
          else
            []
        )
        [ table [style "table-layout" "fixed", style "width" "100%"]
          [ tr [] 
            [ td [colspan 2]
                 [ div [class "ccInfo"] <| viewDesignInfo size cfg design]
            ]
          , tr []
            [ td [] [viewCC design.design]
            , td [class "rightcell"]
              [ text ("To link to this design: [link design:" ++ (idStr design.design.designid) ++ "] ... [/link] ")
              ]
            ]
          , tr []
            [ td [class "halfcell"]
              [ div [class "filediv"]
                [ design.noteshtml
                , design.cfdghtml
                ]
              , div [class "disclaimer"]
                [ text """
  An uploaded image and the corresponding CFDG are both presumed to be owned by the uploader. 
  The CFDG Gallery makes no claims about ownership whatsoever. Peace!
  """
                ]
              ]
            , td [class "commentcell"]
              ( let
                  editing = List.any Comment.isEditable design.comments
                  user = if editing then Nothing else cfg.currentUser
                  commentcfg = Comment.ViewConfig user cfg.commentToDelete
                in
                  [ div [class "commentsdiv"]
                    (List.intersperse (hr [][])
                      (List.map ((Comment.view commentcfg) >> (Html.map (commentMsgId design.design.designid)))
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
      , if addHRs then
          hr [] []
        else
          text ""
      ]
    Medium ->
      div 
      [ style "overflow" "auto"
      , style "position" "relative"
      , style "min-height" String.fromInt (minHeight design.design) ++ "px"
      , id ("design" ++ (idStr design.design.designid))
      ]
      [ div (thumbImageAttributes design.design)
        [ thumbImage design.design ]
      , div [style "padding-left" "310px"]
        [ div [style "padding-left" "5px"] <| viewDesignInfo size cfg design
        , div [class "filediv", style "width" "95%"]
              [ design.noteshtml
              , div [class "minicfdg"] [design.cfdghtml]
              ]
        ]
      ]
    Small ->
      table [class "med_thumbtable", id ("design" ++ (idStr design.design.designid))]
        [ tr []
          [ td [class "med_thumbcell"]
            [ a [ href <| "#design/" ++ (idStr design.design.designid), onNav (FocusClick, design.design.designid) ]
              [ img [ class "image", src design.design.thumblocation, alt "design thumbnail"] []]
            ]
          ]
        , tr []
          [ td [] (viewDesignInfo size cfg design)
          ]
        ]
    Mini ->
      table [class "sm_thumbtable", id ("design" ++ (idStr design.design.designid))]
        [ tr []
          [ td [class "sm_thumbcell"]
            [ thumbLink cfg "" design.design.designid
              [ img [ class "image", src design.design.smthumblocation, alt "design thumbnail"] []]
            ]
          , td [class "sm_thumbinfo"] (viewDesignInfo size cfg design)
          ]
        ]

tagDeleteLink : String -> List (Html EMsg)
tagDeleteLink tag =
  [ text " "
  , a [href (makeUri "#tag" [tag, "0"]), target "_blank"] [text tag]
  , text " "
  , a [href "#", onNav (TagDelete tag), title "Delete this tag", class "tagbutton"] [text "x"]
  ]

tagOptions : List TagInfo -> List (Html EMsg)
tagOptions tags =
  let
    opt tagi = option [value tagi.name, class "tagoption", onClick (TagSelect tagi.name)] [text tagi.name]
    tags_ = if List.length tags == 1 then tags ++ [TagInfo " " 0] else tags
  in
    List.map opt tags_

viewTagEdit : List TagInfo -> EditDesign -> Html EMsg
viewTagEdit tags edesign =
  tr []
    [ td [] [b [] [text "Tags"], text ":"]
    , td [colspan 2] 
      (( input 
        [ type_ "text", size 12, name "tags"
        , value edesign.newTag
        , autocomplete False
        , onFocus (TagFocus True), onBlur (TagFocus False)
        , onInput TagType
        ] []
       ):: [text " ", a [href "#", onNav TagAdd, title "Add this tag", class "tagbutton"] [text "add"]]
        ++ (List.concat (List.map tagDeleteLink edesign.design.tags))
        ++ [ let
                match tag tagi = String.contains tag tagi.name
                matching = List.filter (match edesign.newTag) tags
                show =  (edesign.focusTag || edesign.focusTaglist) && 
                        (not edesign.tagSelected) &&
                        (not (List.isEmpty matching))
              in
                div 
                  [ id "taglist", hidden (not show)]
                  [ select 
                    [ size (clamp 2 10 (List.length matching))
                    , required False
                    , onFocus (TagsFocus True), onBlur (TagsFocus False)
                    ]
                    (tagOptions matching)
                  ]
           ]
      )
    ]

viewEdit : List TagInfo -> EditDesign -> Html EMsg
viewEdit tags edesign =
  div []
  [ if edesign.design.designid == nonDesign then
      h1 [] [text "Upload your artwork!"]
    else
      h1 [] [text "Update your artwork!"]
  , div [style "width" "600px"]
    [ text "Pick a title for your artwork (e.g. \"People Dancing and Dying\") "
    , text "and upload it here. If you are using "
    , b [] [text "Context Free"]
    , text " for Mac or Windows you can upload directly from the program."
    , br [][]
    , br [][]
    ]
  , Html.form [method "POST", action "#", 
          enctype "multipart/form-data", onSubmit Upload]
    [ table [class "upload"]
      [ tr []
        [ td [] [b [] [text "Title"], text ":"]
        , td [] [input [type_ "text", size 30, maxlength 100, name "title"
                , attribute "data-lpignore" "true"
                , value edesign.design.title, onInput TitleChange][]]
        , td 
          [ class "alert"
          , style "visibility" 
                  if (validateTitle edesign.design.title) then
                    "hidden"
                  else
                    "visible"
          ] [text "Title must be between 3 and 100 characters."]
        ]
      , tr []
        [ td [] [b [] [text "CFDG"], text " file:"]
        , td [] [ input [type_ "file", name "cfdgfile", id "cfdgfile"
                , on "change" (JD.succeed (FileChange "cfdgfile"))][]]
        , td 
          [ class (if (validateCfdg edesign) then "foo" else "alert")]
          [ if validateCfdg edesign then
              if (edesign.fileSelected) then
                text " "
              else
                text "Existing CFDG file will be kept."
            else
              text "CFDG file must be chosen."
          ]
        ]
      , tr []
        [ td [] [b [] [text "PNG"], text " file:"]
        , td [] [ input [type_ "file", name "imagefile", id "imagefile"
                , on "change" (JD.succeed (FileChange "imagefile"))] []]
        , td 
          [ class (if (validateImage edesign) then "foo" else "alert")]
          [ if validateImage edesign then
              if (edesign.imageSelected) then
                text " "
              else
                text "Existing image file will be kept."
            else
              text "PNG file must be chosen."
          ]
        ]
      , tr [] 
        [ td [] [text "Image upload compression type:"]
        , td [] 
          [ input 
            [ type_ "radio"
            , name "compression"
            , value "JPEG"
            , checked (not edesign.uploadPNG)
            , onClick (PNGchoose False)
            ][]
          , text " JPEG "
          , input 
            [ type_ "radio"
            , name "compression"
            , value "PNG-8"
            , checked (edesign.uploadPNG)
            , onClick (PNGchoose True)
            ][]
          , text " PNG-8 "
          ]
        , td [][]
        ]
      , tr []
        [ td [] [b [] [text "Variation"], text ":"]
        , td [] [ input [type_ "text", size 9, maxlength 6, name "variation"
                , value edesign.design.variation
                , onInput VariationChange
                ][]]
        , td 
          [ class "alert"
          , style "visibility"
                  if (validateVariation edesign.design.variation) then
                    "hidden"
                  else
                    "visible"
          ] [text "Variation must be 0 to 6 letters."]
        ]
      , viewTagEdit tags edesign
      , tr []
        [ td [] [text "Design is ", b[] [text "tiled"], text " or ", b [][text "frieze"], text ":"]
        , td [] 
          [ select [name "tiledtype", size 1, onSelect TiledChange]
            [ option [value "0", selected (edesign.design.tiled == Untiled)] [text "Not tiled"]
            , option [value "1", selected (edesign.design.tiled == Hfrieze)] [text "Horizontal frieze"]
            , option [value "2", selected (edesign.design.tiled == Vfrieze)] [text "Vertical frieze"]
            , option [value "3", selected (edesign.design.tiled == Tiled)]   [text "Tiled"]
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
          [ textarea
            [ rows 5, cols 20, maxlength 1000
            , name "notes", value edesign.design.notes
            , onInput NotesChange
            ] []
          , tagHelp
          ]
        ]
      , tr []
        [ td [class "vupload"] [b [] [text "Set License"], text ":"]
        , td [colspan 2]
          [ select [name "cclicense", size 1, onSelect CCchange]
            [ option (makeSelectAttrs "-" edesign.ccLicense) [text "No change"]
            , optgroup [attribute "label" "Public Domain"]
              [ option (makeSelectAttrs "zero" edesign.ccLicense) [text "Creative Commons Zero"]]
            , optgroup [attribute "label" "Creative Commons Licenses"]
              [ option (makeSelectAttrs "by" edesign.ccLicense) [text "Creative Commons Attribution"]
              , option (makeSelectAttrs "by-sa" edesign.ccLicense) [text "Creative Commons Attribution-ShareAlike"]
              , option (makeSelectAttrs "by-nd" edesign.ccLicense) [text "Creative Commons Attribution-NoDerivatives"]
              , option (makeSelectAttrs "by-nc" edesign.ccLicense) [text "Creative Commons Attribution-NonCommercial"]
              , option (makeSelectAttrs "by-nc-sa" edesign.ccLicense) [text "Creative Commons Attribution-NonCommercial-ShareAlike"]
              , option (makeSelectAttrs "by-nc-nd" edesign.ccLicense) [text "Creative Commons Attribution-NonCommercial-NoDerivatives"]
              ]
            , optgroup [attribute "label" "Default Copyright"]
              [ option [value ""][text "All Rights Reserved"]]
            ]
          ]
        ]
      , tr []
        [ td [][text "Current License:"]
        , td [colspan 2] [viewCC edesign.design]
        ]
      , tr []
        [ td [] 
          [ input 
            [ type_ "submit"
            , value
                (if edesign.design.designid == nonDesign then
                  "Upload Design"
                else
                  "Update Design")
            , disabled (not (validDesign edesign))
            ] []
          , text " "
          , input [type_ "submit", value "Cancel", onNav CancelEdit] []
          ]
        , td [colspan 2] 
          [ input [type_ "hidden", name "designid", value (idStr edesign.design.designid)] []
          ]
        ]
      ]
    ]
  ]

viewEditTags : List TagInfo -> EditDesign -> Html EMsg
viewEditTags tags edesign =
  div []
  [ h1 [] [text "Edit the tags for this design:"]
  , Html.form [method "POST", action "#", 
          enctype "multipart/form-data", onSubmit UploadTags]
    [ table []
      [ tr []
        [ td [colspan 3, class "fullimagediv"]
          [ img [class "image", src edesign.design.imagelocation, alt "cfdg image"] [] ]
        ]
      , tr []
        [ td [] [b [] [text "Title"], text ":"]
        , td [] [input [type_ "text", size 30, maxlength 100, name "title"
                , attribute "data-lpignore" "true"
                , value edesign.design.title
                , readonly True][]]
        , td [] []
        ]
      , viewTagEdit tags edesign
      , tr []
        [ td [] 
          [ input 
            [ type_ "submit"
            , value "Update Tags"
            ] []
          , text " "
          , input [type_ "submit", value "Cancel", onNav CancelEdit] []
          ]
        , td [colspan 2] 
          [ input [type_ "hidden", name "designid", value (idStr edesign.design.designid)] []
          ]
        ]
      ]
    ]
  ]
