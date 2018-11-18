import Html exposing (..)
import Array exposing (Array)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onSubmit, onInput, custom, on)
import String exposing (trimLeft, toInt)
import Login
import Design
import Comment
import User exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Http
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>), s, int, top)
import Url.Parser.Query as UrlQ
import Random
import GalleryUtils exposing (..)
import Time exposing (Posix)
import Task
import Ports exposing (..)
import Browser.Dom
import Dict exposing (Dict)
import File

main =
  Browser.application
    { init = initModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = NewURL
    }


-- MODEL

type ViewMode
  = Designs
  | Tags
  | People
  | Translation
  | Default
  | Editing
  | EditingTags
  | Error

type alias DesignList =
  { designs : Array Design.DisplayDesign
  , prevlink : String
  , nextlink : String
  , thislink : String
  , start : Int
  , count : Int
  , currentHash : String
  }

type alias UserList =
  { users : List User.MiniUser
  , prevlink : String
  , nextlink : String
  , thislink : String
  , count : Int
  }

type alias CommentList =
  { comments : List Comment.Comment
  , design : DesignID
  }

type alias FaveInfo =
  { designid : DesignID
  , fans : List String
  }

type alias DesignInfo =
  { fans : List String
  , tags : List String
  , imagesize : Maybe Design.Size
  }

type alias DesignTags =
  { design : Design.DisplayDesign
  , tags : Maybe (List TagInfo)
  }

type alias Flags =
  { backend : String
  }

type OrderT = ASC | DESC | None

type alias UserOrder =
  { name : OrderT
  , posts : OrderT
  , join : OrderT
  }

type alias Cfdg3Info =
  { text : String
  , targeted : Bool
  , discards : Bool
  }

type Session 
  = NotLoggedIn String
  | LoginPending
  | LoggedIn User

type alias Model =
  { user : Session
  , loginform : Login.Model
  , key : Nav.Key
  , title : String
  , limitCC : Bool
  , authorLookup : String
  , designLookup : Int
  , viewMode : ViewMode
  , mainDesign : DesignID
  , designToDelete : DesignID
  , commentToDelete : CommentID
  , designList : DesignList
  , miniLists : Dict String (List Design.DisplayDesign)
  , miniSeed : Int
  , pendingLoad : Bool
  , editDesign : Maybe Design.EditDesign
  , designMode : Design.ViewSize
  , showFans : Bool
  , tagList : List TagInfo
  , userList : UserList
  , userOrder : UserOrder
  , errorMessage : String
  , initUrl : String
  , currentHash : String
  , errorInfo : Result Http.Error String
  , backend : String
  , cfdg2text : String
  , cfdg3text : String
  }

zeroList : DesignList
zeroList = DesignList Array.empty "" "" "" 0 0 ""

zeroUList : UserList
zeroUList = UserList [] "" "" "" 0

initModel : String -> Url -> Nav.Key -> (Model, Cmd Msg)
initModel backend loc key = 
  ( { user              = LoginPending
    , loginform         = Login.initModel
    , key               = key
    , title             = "Gallery"
    , limitCC           = False
    , authorLookup      = ""
    , designLookup      = 0
    , viewMode          = Designs
    , mainDesign        = nonDesign
    , designToDelete    = nonDesign
    , commentToDelete   = noComment
    , designList        = zeroList
    , miniLists         = Dict.empty
    , miniSeed          = 0
    , pendingLoad       = False
    , editDesign        = Nothing
    , designMode        = Design.Mini
    , showFans          = False
    , tagList           = []
    , userList          = zeroUList
    , userOrder         = UserOrder ASC None None
    , errorMessage      = ""
    , initUrl           = Url.toString loc
    , currentHash       = Maybe.withDefault "" loc.fragment
    , errorInfo         = Ok ""
    , backend           = backend
    , cfdg2text         = ""
    , cfdg3text         = ""
    }
  , loginSession backend
  )

designMap : (Design.DisplayDesign -> Design.DisplayDesign) -> DesignList -> DesignList
designMap mapping oldList =
  { oldList | designs = Array.map mapping oldList.designs }

nonDesignIndex : Int
nonDesignIndex = -1

designFindHelper : Int -> DesignID -> (Array Design.DisplayDesign) -> (Int, Maybe Design.DisplayDesign)
designFindHelper =
  \index id darray ->
    if index >= (Array.length darray) then
      (nonDesignIndex, Nothing)
    else
      case Array.get index darray of
        Nothing -> (nonDesignIndex, Nothing)
        Just design ->
          if design.design.designid == id then
            (index, Just design)
          else
            designFindHelper (index + 1) id darray
      

designFind : DesignID -> (Array Design.DisplayDesign) -> (Int, Maybe Design.DisplayDesign)
designFind = designFindHelper 0
      
dmerge : String -> DesignList -> DesignList -> (DesignList, DesignID)
dmerge hash new old =
  let
    newparts = String.split "/" new.thislink
    oldparts = String.split "/" old.thislink
    name_ = String.dropLeft 1 hash
    mfront = Array.get 0 new.designs
    mback = Array.get (Array.length new.designs - 1) new.designs
    frontid = case mfront of 
      Just dd -> dd.design.designid 
      Nothing -> nonDesign
    backid = case mback of 
      Just dd -> dd.design.designid 
      Nothing -> nonDesign
  in case (List.head newparts, List.head oldparts) of
    (Just newhead, Just oldhead) -> 
      if (newhead /= oldhead) then
        ({new | currentHash = hash}, nonDesign)
      else if old.start + old.count == new.start then
        ({old | designs = Array.append old.designs new.designs
              , nextlink = new.nextlink
              , count = old.count + new.count
              , currentHash = hash}, nonDesign)
      else if new.start + new.count == old.start then
        ({old | designs = Array.append new.designs old.designs
              , prevlink = new.prevlink
              , count = old.count + new.count
              , start = new.start
              , thislink = new.thislink
              , currentHash = hash}, backid)
      else
        ({new | currentHash = hash}, nonDesign)
    _ ->
      ({new | currentHash = hash}, nonDesign)
      

-- URL PARSING


type Route
  = Home
  | Login String String String
  | ErrorMsg String
  | DesignByID Int
  | EditDesign Int
  | EditTags Int
  | Author String Int Int
  | AuthorInit String Int
  | AuthorInit2 String
  | Faves String Int Int
  | FavesInit String Int
  | Newest Int Int
  | NewestInit Int
  | Oldest Int Int
  | OldestInit Int
  | Title Int Int
  | TitleInit Int
  | TitleIndex String
  | Popular Int Int
  | PopularInit Int
  | RandomDes Int Int Int
  | RandomInit Int Int
  | RandomSeed
  | Tag String Int Int
  | TagInit String Int
  | ShowTags String
  | Users String Int Int
  | ShowTranslate Int





router : Url.Parser (Route -> a) a
router =
  Url.oneOf
    [ Url.map Home top
    , Url.map Login (Url.s "login" </> Url.string </> Url.string </> Url.string)
    , Url.map ErrorMsg (Url.s "error" </> Url.string)
    , Url.map DesignByID (Url.s "design" </> int)
    , Url.map EditDesign (Url.s "edit" </> int)
    , Url.map EditTags (Url.s "edittags" </> int)
    , Url.map Author (Url.s "user" </> Url.string </> int </> int)
    , Url.map AuthorInit (Url.s "user" </> Url.string </> int)
    , Url.map AuthorInit2 (Url.s "user" </> Url.string)
    , Url.map Faves (Url.s "faves" </> Url.string </> int </> int)
    , Url.map FavesInit (Url.s "faves" </> Url.string </> int)
    , Url.map Newest (Url.s "newest" </> int </> int)
    , Url.map NewestInit (Url.s "newest" </> int)
    , Url.map Oldest (Url.s "oldest" </> int </> int)
    , Url.map OldestInit (Url.s "oldest" </> int)
    , Url.map Title (Url.s "title" </> int </> int)
    , Url.map TitleInit (Url.s "title" </> int)
    , Url.map TitleIndex (Url.s "titleindex" </> Url.string)
    , Url.map Popular (Url.s "popular" </> int </> int)
    , Url.map PopularInit (Url.s "popular" </> int)
    , Url.map RandomDes (Url.s "random" </> int </> int </> int)
    , Url.map RandomInit (Url.s "random" </> int </> int)
    , Url.map RandomSeed (Url.s "random")
    , Url.map Tag (Url.s "tag" </> Url.string </> int </> int)
    , Url.map TagInit (Url.s "tag" </> Url.string </> int)
    , Url.map ShowTags (Url.s "tags" </> Url.string)
    , Url.map Users (Url.s "users" </> Url.string </> Url.int </> Url.int)
    , Url.map ShowTranslate (Url.s "translate" </> Url.int)
    ]




-- UPDATE

type HttpError18
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus Int String
  | BadPayload String Int String

type Msg
  = LogoutClick
  | CCcheck Bool
  | SwitchTo Design.ViewSize
  | NewURL Url.Url
  | LinkClicked Browser.UrlRequest
  | LoadDesigns String
  | LoginMsg Login.Msg
  | DesignMsg Design.MsgId
  | EDesignMsg Design.EMsg
  | LookupName
  | LookupDesign
  | AuthorText String
  | DesignText String
  | Cfdg2Change String
  | NewSeed Int
  | NewMiniSeed Int Int
  | LoadDesign Posix
  | NewDesign (Result Http.Error DesignTags)
  | NewEditDesign (Result Http.Error Design.EditDesign)
  | NewDesigns (Result Http.Error DesignList)
  | NewUser (Result Http.Error User.User)
  | SessionUser (Result Http.Error User.User)
  | LogoutUser (Result Http.Error Bool)
  | ReceiveCfdg DesignID (Result Http.Error String)
  | ReceiveInfo DesignID (Result Http.Error DesignInfo)
  | NewComments (Result Http.Error CommentList)
  | NewComment (Result Http.Error Comment.Comment)
  | RemoveComment (Result Http.Error CommentID)
  | GotTitleIndex (Result Http.Error Int)
  | GotTags (Result Http.Error (List TagInfo))
  | NewUsers (Result Http.Error UserList)
  | NewFaves (Result Http.Error FaveInfo)
  | DeleteADesign (Result Http.Error DesignID)
  | UploadResponse (Result HttpError18 DesignTags)
  | NewCfdg3 (Result HttpError18 Cfdg3Info)
  | DesignStatusUpdated (Result Http.Error Bool)
  | NewMiniList String (Result Http.Error (List Design.DisplayDesign))
  | ReceiveUnseen (Result Http.Error Int)
  | GetCFAWidth (Result Browser.Dom.Error Browser.Dom.Element)
  | TryScroll String
  | TranslateText
  | Cfdg2Read String
  | TranslateFile (List File.File)
  | TickTock Posix
  | IsVisible String


updateDesigns : String -> Model -> String -> Int -> Int -> (Model, Cmd Msg)
updateDesigns title model query start count =
  let
    go2server = model.currentHash /= model.designList.currentHash ||
                model.currentHash == ""
  in
    ( { model | mainDesign = nonDesign
              , viewMode = Designs
              , pendingLoad = go2server
              , showFans = False
              , title = "Gallery - " ++ title ++ " designs"
      }
    , if go2server then
        getDesigns model query start count
      else
        Cmd.none
    )

scrollToDesign : DesignID -> Cmd Msg
scrollToDesign id = 
  if id == nonDesign then
    Cmd.none
  else
    scrollToElement <| "design" ++ (idStr id)

errorModel : HttpError18 -> String -> Model -> Model
errorModel error context model =
  let
    msg = case error of
      BadStatus _ body -> 
        let
          i = String.indices "<p>" body
          j = String.indices "</p>" body
        in case (List.head i, List.head j) of
          (Just start, Just end) ->
            context ++ " issue: " ++ (String.slice (start + 3) end body)
          _ -> body
      BadPayload jmsg code body -> 
        "Unexpected response."
      _ -> "Communication error."
    httpError = case error of
      BadUrl url -> Http.BadUrl url
      Timeout -> Http.Timeout
      NetworkError -> Http.NetworkError
      BadStatus code _ -> Http.BadStatus code
      BadPayload jmsg _ _ -> Http.BadBody jmsg
  in
    {model | errorInfo = Err httpError, errorMessage = msg}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LogoutClick ->
      (model, logoutUser model)
    CCcheck cc ->
      let
        model_ = {model | limitCC = cc}
        url_ = (makeQuery model_) ++ "#" ++ model_.designList.thislink
      in
        (model_, Nav.pushUrl model_.key url_)
    SwitchTo size ->
      let
        model_ = {model | designMode = size}
        url_ = (makeQuery model_) ++ "#" ++ model_.designList.thislink
      in
        (model_, Nav.pushUrl model_.key url_)
    LoadDesigns newHash ->
      (model, Nav.replaceUrl model.key ("#" ++ newHash))
    NewURL loc ->
      let
        search = Maybe.withDefault "" loc.query
        qbits = String.split "&" search
        cc_ = List.member "cc" qbits
        mode_ = if (List.member "large" qbits) then Design.Medium 
                else if (List.member "medium" qbits) then Design.Small
                else Design.Mini
        hash = Maybe.withDefault "" loc.fragment
        model_ = {model | limitCC = cc_
                        , designMode = mode_
                        , currentHash = hash}
        dcount = case model_.designMode of
          Design.Mini -> 48
          Design.Small -> 27
          _ -> 5
        loc_ = {loc | path = hash, fragment = Nothing}
      in
        case Url.parse router loc_ of
          Nothing ->
            (model_, Cmd.none)
          Just route ->
            case route of
              Home ->
                let
                  model__ = {model_ | viewMode = Default
                                    , mainDesign = nonDesign
                                    , showFans = False
                                    , designList = zeroList
                                    , miniLists = Dict.empty
                                    , pendingLoad = False
                                    , title = "Gallery"
                            }
                in
                  (model__, Cmd.batch
                    [ Task.attempt GetCFAWidth <| Browser.Dom.getElement "CFAcontent"
                    , checkUnseen model__
                    ])
              Login user password remember ->
                let
                  err = case (user, password) of
                    ("", _) -> "Username required"
                    (_, "") -> "Password required"
                    _ -> ""
                  login_ = Login.Model user password (remember /= "0")
                  model__ = {model_ | loginform = login_}
                in
                  if err == "" then
                    ( {model__ | user = LoginPending}
                    , Cmd.batch [loginUser model__, Nav.replaceUrl model__.key "#"]
                    )
                  else
                    ({model__ | user = NotLoggedIn err}, Nav.replaceUrl model__.key "#")
              ErrorMsg msg_enc ->
                let
                  emsg = Maybe.withDefault "Malformed error message." (Url.percentDecode msg_enc)
                in
                  ( {model_ | errorMessage = emsg
                            , viewMode = Error
                            , title = "Gallery - Error"
                    }, Cmd.none)
              DesignByID id ->
                let
                  designId = ID id
                  (_, mddesign) = designFind designId model_.designList.designs
                in case mddesign of
                  Nothing -> 
                    ( {model_ | mainDesign = nonDesign
                              , showFans = False
                              , viewMode = Designs
                              , designList = zeroList
                              , pendingLoad = True
                              , title = "Gallery - Design"
                       }
                    , getDesign designId model_
                    )
                  Just ddesign ->
                    ( {model_ | mainDesign = designId
                              , showFans = False
                              , viewMode = Designs
                              , title = "Gallery - Design: " ++ ddesign.design.title
                      }
                    , Cmd.batch
                      [ getComments designId model_
                      , getCfdg designId model_
                      , getInfo designId model_
                      , scrollToDesign designId
                      ]
                    )
              EditDesign id ->
                case model_.user of
                  LoggedIn user ->
                    if id == 0 then
                      ( {model_ | editDesign = Nothing, viewMode = Editing, errorMessage = ""}
                      , Task.perform LoadDesign Time.now
                      )
                    else
                      let
                        designid = (ID id)
                        (_, mddesign) = designFind designid model_.designList.designs
                      in case mddesign of
                        Nothing ->
                          ( {model_ | editDesign = Nothing
                                    , viewMode = Editing
                                    , errorMessage = ""
                                    , designList = zeroList
                                    , pendingLoad = True
                                    , title = "Gallery - Edit Design"
                            }
                          , loadEditDesign designid model_
                          )
                        Just ddesign ->
                          ( {model_ | editDesign = Just <| Design.makeEDesign ddesign.design
                                    , viewMode = Editing
                                    , errorMessage = ""
                                    , title = "Gallery - Edit Design: " ++ ddesign.design.title
                            }
                          , getInfo designid model_
                          )
                  _ -> (model_, Cmd.none)
              EditTags id ->
                case model_.user of
                  LoggedIn user ->
                    let
                      designid = (ID id)
                      (_, mddesign) = designFind designid model_.designList.designs
                    in case mddesign of
                      Nothing ->
                        ( {model_ | editDesign = Nothing
                                  , viewMode = EditingTags
                                  , errorMessage = ""
                                  , designList = zeroList
                                  , pendingLoad = True
                                  , title = "Gallery - Edit Design"
                          }
                        , loadEditDesign designid model_
                        )
                      Just ddesign ->
                        ( {model_ | editDesign = Just <| Design.makeEDesign ddesign.design
                                  , viewMode = EditingTags
                                  , errorMessage = ""
                                  , title = "Gallery - Edit Design: " ++ ddesign.design.title
                          }
                        , getInfo designid model_
                        )
                  _ -> (model_, Cmd.none)
              Author name_enc start count ->
                let
                  name = Maybe.withDefault "" (Url.percentDecode name_enc)
                  namepos = possessive name
                in
                  updateDesigns namepos model_ (makeUri "by" [name]) start count
              AuthorInit name_enc start ->
                let
                  name = Maybe.withDefault "" (Url.percentDecode name_enc)
                in
                  ( {model_  | designList = zeroList }
                  , Nav.replaceUrl model_.key (makeUri "#user" [name, String.fromInt start, String.fromInt dcount]))
              AuthorInit2 name_enc ->
                let
                  name = Maybe.withDefault "" (Url.percentDecode name_enc)
                in
                  (model_, Nav.replaceUrl model_.key (makeUri "#user" [name, "0", String.fromInt dcount]))
              Faves name_enc start count ->
                let
                  name = Maybe.withDefault "" (Url.percentDecode name_enc)
                  namepos = possessive name
                in
                  updateDesigns (namepos ++ " favorite") model_ (makeUri "faves" [name]) start count
              FavesInit name_enc start ->
                let
                  name = Maybe.withDefault "" (Url.percentDecode name_enc)
                in
                  ({model_ | designList = zeroList }
                  , Nav.replaceUrl model_.key (makeUri "#faves" [name, String.fromInt start, String.fromInt dcount]))
              Newest start count ->
                updateDesigns "Latest" model_ "newest" start count
              NewestInit start ->
                let
                  user_ = case model_.user of
                    LoggedIn u -> LoggedIn {u | unseen = 0}
                    _ -> model_.user
                in
                  ({model_ | designList = zeroList, user = user_}
                  , Nav.replaceUrl model_.key (makeUri "#newest" [String.fromInt start, String.fromInt dcount]))
              Oldest start count ->
                updateDesigns "Oldest" model_ "oldest" start count
              OldestInit start ->
                ({model_ | designList = zeroList }
                , Nav.replaceUrl model_.key (makeUri "#oldest" [String.fromInt start, String.fromInt dcount]))
              Title start count ->
                updateDesigns "Title" model_ "title" start count
              TitleInit start ->
                ({model_ | designList = zeroList }
                , Nav.replaceUrl model_.key (makeUri "#title" [String.fromInt start, String.fromInt dcount]))
              TitleIndex title ->
                (model_, getTitle model_ title)
              Popular start count ->
                updateDesigns "Likes" model_ "popular" start count
              PopularInit start ->
                ({model_ | designList = zeroList }
                , Nav.replaceUrl model_.key (makeUri "#popular" ["0", String.fromInt dcount]))
              RandomDes seed start count ->
                updateDesigns "Random" model_ ("random/" ++ (String.fromInt seed)) start count
              RandomInit seed start ->
                ({model_ | designList = zeroList }
                , Nav.replaceUrl model_.key (makeUri "#random" [String.fromInt seed, "0", String.fromInt dcount]))
              RandomSeed ->
                (model_, Random.generate NewSeed (Random.int 1 1000000000))
              Tag tag_enc start count ->
                let
                  tag = Maybe.withDefault "" (Url.percentDecode tag_enc)
                in
                  updateDesigns ("Tag " ++ tag) model_ (makeUri "tag" [tag]) start count
              TagInit tag_enc start ->
                let
                  tag = Maybe.withDefault "" (Url.percentDecode tag_enc)
                in
                  ({model_ | designList = zeroList }
                  , Nav.replaceUrl model_.key (makeUri "#tag" [tag, "0", String.fromInt dcount]))
              ShowTags tagType ->
                let
                  comp = 
                    if tagType == "tag" then
                      \a b -> compare a.name b.name
                    else
                      \a b -> 
                        if a.count == b.count then
                          compare a.name b.name
                        else
                          compare b.count a.count   -- descending order
                in
                  ( { model_| tagList = List.sortWith comp model_.tagList
                            , viewMode = Tags
                            , title = "Gallery - Tag list"
                    }
                  , Cmd.none
                  )
              Users utype start count ->
                let
                  (userOrder_, valid) = case utype of
                    "name"     -> (UserOrder ASC  None None, True)
                    "posts"    -> (UserOrder None ASC  None, True)
                    "joined"   -> (UserOrder None None ASC , True)
                    "name_d"   -> (UserOrder DESC None None, True)
                    "posts_d"  -> (UserOrder None DESC None, True)
                    "joined_d" -> (UserOrder None None DESC, True)
                    _          -> (UserOrder None None None, False)
                in
                  if valid then
                    ( {model_ | viewMode = People
                              , userOrder = userOrder_
                              , title = "Gallery - User list"
                      }
                    , getUsers ("users/" ++ utype) start count model_
                    )
                  else
                    (model_, Cmd.none)
              ShowTranslate idint ->
                ( {model_ | viewMode = Translation
                          , cfdg3text = ""
                          , errorMessage = ""
                          , title = "Gallery - Translation"
                  }
                , if idint > 0 then
                    translateDesign (ID idint) model_
                  else
                    Cmd.none
                )
    LinkClicked req -> 
      ( model
      , case req of
          Browser.Internal url ->
            if String.contains "gallery2" url.path then
              Nav.pushUrl model.key <| Url.toString url
            else
              Nav.load (Url.toString url)
          Browser.External str ->
            Nav.load str
      )
    NewSeed seed ->
      (model, Nav.replaceUrl model.key ("#random/" ++ (String.fromInt seed ++ "/0")))
    LoginMsg lMsg ->
      let
        newLoginModel = Login.update lMsg model.loginform          
      in
        ({model | loginform = newLoginModel}, Cmd.none)
    DesignMsg (dmsg, id) ->
      let
        (index, mddesign) = designFind id model.designList.designs
        designList = model.designList
      in
        case mddesign of
          Nothing -> (model, Cmd.none)
          Just ddesign ->
            let
              (ddesign_, act_) = Design.update dmsg ddesign
              designList_ = {designList | designs = Array.set index ddesign_ model.designList.designs}
              model_ = {model | designList = designList_}
            in case act_ of
              Just (DeleteDesign id_) ->
                if id_ == model_.designToDelete && id_ /= nonDesign then
                  (model_, deleteDesign id_ model)
                else
                  ({model_ | designToDelete = id_}, Cmd.none)
              Just (DeleteComment commentid) ->
                if commentid == model_.commentToDelete && commentid /= noComment then
                  (model_, deleteComment commentid model)
                else
                  ({model_ | commentToDelete = commentid}, Cmd.none)
              Just (CloseDesign) ->
                ( {model_ | mainDesign = nonDesign, showFans = False}
                , Cmd.batch
                  [ Nav.back model_.key 1
                  , scrollToDesign model_.mainDesign
                  ]
                )
              Just (CancelEditAct) -> case model_.editDesign of
                Nothing ->
                  ({model_ | mainDesign = nonDesign}, Nav.back model_.key 1)
                Just edesign ->
                  ( {model_ | mainDesign = edesign.design.designid}
                  , Cmd.batch
                    [ Nav.back model_.key 1
                    , scrollToDesign edesign.design.designid
                    ]
                  )
              Just (Focus id_) -> 
                ( {model_ | mainDesign = id_, showFans = False}
                , if model_.mainDesign == nonDesign then
                    Nav.pushUrl model_.key ("#design/" ++ (idStr id_))
                  else
                    Nav.replaceUrl model_.key ("#design/" ++ (idStr id_))
                )
              Just (ShowFans show) ->
                ({model_ | showFans = show}, Cmd.none)
              _ -> (model_, resolveAction act_ model_)
    EDesignMsg emsg -> case model.editDesign of
      Nothing -> (model, Cmd.none)
      Just edesign ->
        let
          (edesign_, act_) = Design.editupdate emsg edesign
          model_ = {model | editDesign = Just edesign_}
        in
          (model_, resolveAction act_ model_)
    LookupName ->
      (model, Nav.pushUrl model.key (makeUri "#user" [model.authorLookup, "0"]))
    LookupDesign ->
      (model, Nav.pushUrl model.key ("#design/" ++ (String.fromInt model.designLookup)))
    AuthorText author ->
      ({ model | authorLookup = trimLeft author }, Cmd.none)
    Cfdg2Change cfdg2 ->
      ({model | cfdg2text = cfdg2}, Cmd.none)
    DesignText desText ->
      let
        des = Maybe.withDefault 0 (String.toInt desText)
      in
        if des < 1  && desText /= "" then
          (model, Cmd.none)
        else
          ({model | designLookup = des}, Cmd.none)
    LoadDesign time ->
      case model.user of
        LoggedIn user ->
          let
            edesign_ = Design.initDesign user.name user.defaultccURI time
          in
            ({model | editDesign = Just edesign_}, Cmd.none)
        _ -> (model, Cmd.none)
    NewDesign designResult ->
      case designResult of
        Ok dt ->
          let
            design = dt.design
            tags_ = Maybe.withDefault model.tagList dt.tags
            designList_ = DesignList (Array.fromList [design]) "" "" "" 0 1 ""
            id = design.design.designid
          in
            ( { model | designList = designList_
                      , tagList = tags_
                      , mainDesign = id
                      , showFans = False
                      , pendingLoad = False
                      , errorInfo = Ok "New design"
                      , title = "Gallery - Design: " ++ design.design.title
              }
            , Cmd.batch
              [ getComments id model
              , getCfdg id model
              ]
            )
        Err error ->
          ( { model | mainDesign = nonDesign
                    , showFans = False
                    , errorInfo = Err error
                    , pendingLoad = False}
          , Cmd.none
          )
    NewEditDesign designResult ->
      let
        stillPending = if model.viewMode == Default then model.pendingLoad else False
      in
        case designResult of
          Ok design ->
            ( { model | editDesign = Just design
                      , pendingLoad = stillPending
                      , errorInfo = Ok "New edit design"
                      , title = "Gallery - Edit Design: " ++ design.design.title
              }
            , Cmd.none 
            )
          Err error ->
            ( { model | editDesign = Nothing
                      , errorInfo = Err error
                      , pendingLoad = stillPending}
            , Cmd.none
            )
    NewDesigns designResult ->
      case designResult of
        Ok designs ->
          let
            (merger, designid) = dmerge model.currentHash designs model.designList
            scroll = scrollToDesign designid
          in
            ( { model | designList = merger
                      , mainDesign = nonDesign
                      , showFans = False
                      , pendingLoad = False
                      , errorInfo = Ok "New designs"}
            , if model.designMode == Design.Small then
                scroll
              else
                Cmd.batch <| scroll :: (List.map (getCfdgfromDesign model) (Array.toList designs.designs))
            )
        Err error ->
          ({model | designList = zeroList, errorInfo = Err error, pendingLoad = False}, Cmd.none)
    NewMiniList listType miniListResult ->
      case miniListResult of
        Ok miniList_ ->
          ({model | miniLists = Dict.insert listType miniList_ model.miniLists}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    TryScroll _ ->
      (model, Cmd.none)
    NewUser loginResult ->
      case loginResult of
        Ok user ->
          ( {model | user = LoggedIn user, errorInfo = Ok "Login success"}
          , Nav.replaceUrl model.key "#newest/0"
          )
        Err error ->
          ( {model | user = NotLoggedIn "login failed", errorInfo = Err error }
          , Cmd.none
          )
    SessionUser loginResult ->
      case loginResult of
        Ok user_ ->
          let
            cmd =
              if user_.unseen > 0 then
                Cmd.batch [getTags model, updateDesignStatus model]
              else
                getTags model
          in
            ({model | user = LoggedIn user_ , errorInfo = Ok "Session loaded"}, cmd)
        Err error ->
          ({model | errorInfo = Err error, user = NotLoggedIn ""}, getTags model)
    LogoutUser logoutResult ->
      case logoutResult of
        Ok yes ->
          ({model | user = NotLoggedIn "", errorInfo = Ok "Logout success"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Nav.pushUrl model.key "#error/Logout%20failed%2e")
    ReceiveCfdg id cfdgResult ->
      case cfdgResult of
        Ok cfdgText ->
          let
            (index, mddesign) = designFind id model.designList.designs
          in case mddesign of
            Nothing -> (model, Cmd.none)     -- Shouldn't happen
            Just ddesign ->
              let
                ddesign_ = Design.setCfdg cfdgText ddesign
                designList = model.designList
                designList_ = {designList | designs = Array.set index ddesign_ designList.designs}
              in
                ({model | designList = designList_, errorInfo = Ok "Cfdg received"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    ReceiveInfo id infoResult ->
      case infoResult of
        Ok info ->
          let
            (index, mddesign) = designFind id model.designList.designs
            editDesign_ = case model.editDesign of
              Just edesign ->
                if edesign.design.designid == id then
                  let
                    design = edesign.design
                    design_ = {design | fans = info.fans
                                      , tags = info.tags
                                      , imagesize = info.imagesize}
                    edesign_ = {edesign | design = design_}
                  in
                    Just edesign_
                else
                  model.editDesign
              Nothing -> model.editDesign
          in case mddesign of
            Nothing -> ({model | editDesign = editDesign_}, Cmd.none)
            Just ddesign ->
              let
                design = ddesign.design
                design_ = {design | fans = info.fans
                                  , tags = info.tags
                                  , imagesize = info.imagesize}
                ddesign_ = {ddesign | design = design_}
                designList = model.designList
                designList_ = {designList | designs = Array.set index ddesign_ designList.designs}
              in
                ( {model  | designList = designList_
                          , editDesign = editDesign_
                          , errorInfo = Ok "Info received"}
                , Cmd.none
                )
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    NewComments commentResult ->
      case commentResult of
        Ok comments ->
          let
            (index, mddesign) = designFind comments.design model.designList.designs
          in case mddesign of
            Nothing -> (model, Cmd.none)  -- Shouldn't happpen
            Just ddesign ->
              let
                ddesign_ = Design.setComments comments.comments ddesign
                designList = model.designList
                designList_ = {designList | designs = Array.set index ddesign_ designList.designs}
              in
                ({model | designList = designList_, errorInfo = Ok "Comments received"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    NewComment commentResult ->
      case commentResult of
        Ok comment -> 
          let
            (index, mddesign) = designFind model.mainDesign model.designList.designs
          in case mddesign of
            Nothing -> (model, Cmd.none)     -- Shouldn't happpen
            Just ddesign ->
              let
                ddesign_ = Design.setComment comment ddesign
                designList = model.designList
                designList_ = {designList | designs = Array.set index ddesign_ designList.designs}
              in
                ({model | designList = designList_, errorInfo = Ok "Comment received"}, Cmd.none)
        Err error -> ({model | errorInfo = Err error}, Cmd.none)
    RemoveComment commentidResult ->
      case commentidResult of
        Ok commentid ->
          let
            (index, mddesign) = designFind model.mainDesign model.designList.designs
          in case mddesign of
            Nothing -> (model, Cmd.none)     -- Shouldn't happpen
            Just ddesign ->
              let
                ddesign_ = Design.removeComment commentid ddesign
                designList = model.designList
                designList_ = {designList | designs = Array.set index ddesign_ designList.designs}
              in
                ({model | designList = designList_, errorInfo = Ok "Comment deleted"}, Cmd.none)
        Err error -> ({model | errorInfo = Err error}, Cmd.none)
    GotTitleIndex indexResult ->
      case indexResult of
        Ok index ->
          ( { model | errorInfo = Ok "Title index"}
          , Nav.pushUrl model.key ("#title/" ++ (String.fromInt index))
          )
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    GotTags tagsResult ->
      let
        cmd = 
          if model.initUrl == "" then
            Cmd.none
          else
            Nav.replaceUrl model.key model.initUrl
      in
        case tagsResult of
          Ok tags ->
            ( {model | tagList = tags
                   , errorInfo = Ok "Tags"
                   , initUrl = ""}
            , cmd
            )
          Err error ->
            ({model | errorInfo = Err error, initUrl = ""}, cmd)
    NewUsers usersResult ->
      case usersResult of
        Ok users ->
          ({model | userList = users, errorInfo = Ok "User list"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    NewFaves favesResults -> 
      case favesResults of
        Ok faveinfo -> 
          let
            (index, mddesign) = designFind faveinfo.designid model.designList.designs
          in case mddesign of
            Just ddesign ->
              if ddesign.design.designid == faveinfo.designid then
                let
                  design = ddesign.design
                  design_ = {design
                             | fans = faveinfo.fans
                             , numvotes = List.length faveinfo.fans}
                  ddesign_ = {ddesign | design = design_}
                  designList = model.designList
                  designList_ = {designList | designs = Array.set index ddesign_ model.designList.designs}
                in
                  ({model | designList = designList_, errorInfo = Ok "New fave list"}, Cmd.none)
              else
                (model, Cmd.none)         -- Shouldn't happen
            Nothing -> (model, Cmd.none)  -- ditto
        Err error -> ({model | errorInfo = Err error}, Cmd.none)
    DeleteADesign designidResult ->
      case designidResult of
        Ok designid ->
          let
            designList = model.designList
            designs_ = Array.filter (\dd -> dd.design.designid /= designid) designList.designs
            designList_ = {designList | designs = designs_}
            cmd_ = 
              if Array.isEmpty designs_ then
                Nav.pushUrl model.key "#"
              else
                Nav.pushUrl model.key ("#" ++ designList_.thislink)
          in
            ( { model | designList = designList_, mainDesign = nonDesign, showFans = False
                    , errorInfo = Ok "Design deleted"}
            , cmd_
            )
        Err error ->
          ({model | errorInfo = Err error}, Nav.pushUrl model.key "#error/Delete%20failed%2e")
    UploadResponse uploadResponse ->
      case uploadResponse of
        Ok dt -> 
          let
            ddesign_ = dt.design
            id = ddesign_.design.designid
            tags_ = Maybe.withDefault model.tagList dt.tags
            designList = model.designList
            (index, _) = designFind id designList.designs
            designs_ = 
              if index == nonDesignIndex then
                Array.fromList [ddesign_]
              else
                Array.set index ddesign_ designList.designs
            designList_ =
              if index == nonDesignIndex then
                DesignList designs_ "" "" "" 0 1 ""
              else
                {designList | designs = designs_}
            cmd_ =
              if index == nonDesignIndex then
                Nav.replaceUrl model.key ("#design/" ++ (idStr ddesign_.design.designid))
              else
                Cmd.batch
                  [ getComments id model
                  , getCfdg id model
                  , getInfo id model
                  , Nav.back model.key 1
                  ]
          in
            ( { model | designList = designList_
                      , editDesign = Nothing
                      , errorInfo = Ok "Upload success"
                      , viewMode = Designs
                      , mainDesign = id
                      , showFans = False
                      , tagList = tags_}
            , cmd_
            )
        Err error ->
          (errorModel error "Upload" model, Cmd.none)
    NewCfdg3 cfdg3response ->
      case cfdg3response of
        Ok cfdg3info -> 
          let
            errorMessage_ = case (cfdg3info.targeted, cfdg3info.discards) of
              ( True, True) -> "Color targets used! Characters were discarded!"
              ( True,False) -> "Color targets used!"
              (False, True) -> "Characters were discarded!"
              (False,False) -> ""
          in
            ({model | cfdg3text = cfdg3info.text, errorMessage = errorMessage_}, Cmd.none)
        Err error ->
          (errorModel error "Translation" model, Cmd.none)
    DesignStatusUpdated updateResult -> case updateResult of
      Ok _ -> (model, Cmd.none)
      Err error -> ({model | errorInfo = Err error}, Cmd.none)
    ReceiveUnseen unseenResult -> 
      case (unseenResult,model.user) of
        (Ok unseen_,LoggedIn u) ->
          ({model | user = LoggedIn {u | unseen = unseen_}}, Cmd.none)
        (Ok _,_) -> (model, Cmd.none)
        (Err error,_) -> ({model | errorInfo = Err error}, Cmd.none)
    GetCFAWidth widthResult ->
      let
        num = case widthResult of
          Ok elem -> Basics.clamp 1 8 ((floor elem.element.width) // 295) 
          Err _ -> 5
      in
        ( model
        , Cmd.batch
          [ getMiniList model "newest" num
          , getMiniList model "newbies" num
          , Random.generate (NewMiniSeed num) (Random.int 1 1000000000)
          ]
        )
    NewMiniSeed num seed ->
      ( {model | miniSeed = seed}
      , Cmd.batch
        [ getMiniList model ("random/" ++ (String.fromInt seed)) num
        , getMiniList model ("popularrandom/" ++ (String.fromInt seed)) num
        ]
      )
    TranslateFile files ->
      case List.head files of
        Nothing -> (model, Cmd.none)
        Just file -> (model, Task.perform Cfdg2Read <| File.toString file)
    Cfdg2Read cfdg2 ->
      ({model | cfdg2text = cfdg2, cfdg3text = "", errorMessage = ""}, translateText model)
    TranslateText -> ({model | cfdg3text = "", errorMessage = ""}, translateText model)
    TickTock _ -> (model, checkVisible "moreplease")
    IsVisible _ -> (model, Nav.replaceUrl model.key ("#" ++ model.designList.nextlink))

makeQuery : Model -> String
makeQuery model =
  case (model.limitCC, model.designMode) of
    (False, Design.Mini)   -> "?"
    (False, Design.Small)  -> "?medium"
    (False, Design.Medium) -> "?large"
    (True,  Design.Mini)   -> "?cc"
    (True,  Design.Small)  -> "?cc&medium"
    (True,  Design.Medium) -> "?cc&large"
    _                      -> ""



-- VIEW



designString: Int -> String
designString des =
  if des < 1 then
    ""
  else
    String.fromInt des

makeIndexLink : Char -> List (Html Msg)
makeIndexLink c =
  [ a [class "letterref", href ("#titleindex/" ++ (String.fromChar c))] 
      [b [] [text (String.fromChar c)]]
  , text " "
  ]

makePNlink : String -> Int -> String -> Html Msg
makePNlink type_ count url =
  a [ href ("#" ++ url), 
      style "visibility" <| if (String.isEmpty url) then "hidden" else "visible"]
    [ b [] [text (type_ ++ " " ++ (String.fromInt count))]]

makeUpBar : Bool -> DesignList -> Html Msg
makeUpBar pending dlist =
  if pending && Array.length dlist.designs > 0 then
    div [class "khomut"] [img [src "graphics/loading.gif?2", alt "No designs", width 64, height 64] []]
  else
    if String.isEmpty dlist.prevlink then
      text ""
    else
      div [class "khomut"]
      [ img
        [ src "graphics/more_up.png"
        , onClick <| LoadDesigns dlist.prevlink
        , alt "More designs"
        , width 64
        , height 64
        ] []]

makeDownBar : Bool -> DesignList -> Html Msg
makeDownBar pending dlist =
  if pending then
    div [class "khomut"] [img [src "graphics/loading.gif?2", alt "No designs", width 64, height 64] []]
  else
    if String.isEmpty dlist.nextlink then
      div [class "khomut"] [img [src "graphics/khomut.png", alt "No designs", width 200, height 64] []]
    else
      div [class "khomut"]
      [ img
        [ src "graphics/more_down.png"
        , onClick <| LoadDesigns dlist.nextlink
        , alt "More designs"
        , width 64
        , height 64
        , id "moreplease"
        ] []]


makeHeader : String -> Html Msg
makeHeader thislink =
  let
    urlparts = String.split "/" thislink
    urlname = Maybe.withDefault "" (List.head <| (List.drop 1) <| urlparts)
    urltype = Maybe.withDefault "" (List.head urlparts)
    name = Maybe.withDefault "What's his name" (Url.percentDecode urlname)
    namepos = possessive name
    designs = namepos ++ " Designs"
    likes = namepos ++ " Likes"
    linktext = "To link to this author: [link user:" ++ name ++ "] ... [/link] "
  in case urltype of
    "title" ->
      div []
        ([ a [class "letterref", href "#title/0"] [b [] [text "all"]], text " "]
        ++
        List.concat (List.map makeIndexLink (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        ++ [hr [][]])
    "faves" ->
      makeTabs 
        [ TabInfo Empty " " ""
        , TabInfo Inactive designs <| makeUri "#user" [urlname, "0"]
        , TabInfo Active likes ""
        , TabInfo Rest linktext ""
        ]
    "user" ->
      makeTabs 
        [ TabInfo Empty " " ""
        , TabInfo Active designs ""
        , TabInfo Inactive likes <| makeUri "#faves" [urlname, "0"]
        , TabInfo Rest linktext ""
        ]
    _ ->
      text ""

makeViewConfig : Model -> Design.ViewSize -> Design.ViewConfig
makeViewConfig model size =
  let
    focus = if size == Design.Large then nonDesign else model.mainDesign
    (index, _) = designFind focus model.designList.designs
    prevmdsg = Array.get (index - 1) model.designList.designs
    nextmdsg = Array.get (index + 1) model.designList.designs
    prev = case prevmdsg of
      Nothing -> nonDesign
      Just ddesign -> ddesign.design.designid
    next = case nextmdsg of
      Nothing -> nonDesign
      Just ddesign -> ddesign.design.designid
    muser = case model.user of
      LoggedIn user_ -> Just user_
      _ -> Nothing
    minilist = model.viewMode == Default
  in
    Design.ViewConfig size muser focus prev next model.designToDelete model.commentToDelete minilist model.showFans

viewDesigns : Model -> List (Html Msg)
viewDesigns model =
  (makeHeader model.currentHash)
  ::
  [ (makeUpBar model.pendingLoad model.designList)
  , div [] 
    (let
      vcfg = makeViewConfig model model.designMode
      htmlList = Array.toList
        (Array.map ((Design.view vcfg) >> (Html.map DesignMsg))
          model.designList.designs)
    in br [][] ::
      if model.designMode == Design.Medium then
        (List.intersperse (hr [] []) htmlList)
      else
        htmlList)
  , (makeDownBar model.pendingLoad model.designList)
  ]


viewUsers : Model -> List (Html Msg)
viewUsers model =
  if List.isEmpty model.userList.users then
    [ text "Nothing to show" ]
  else
  [ div [class "clearleft"] 
    [ makePNlink "Previous" model.userList.count model.userList.prevlink
    , text " "
    , makePNlink "Next" model.userList.count model.userList.nextlink
    ]
  , table [class "tagstable"]
    ( [ thead [] 
        [ tr [] 
          [ case model.userOrder.name of
              ASC  -> th [align "left", class "usernames"] [a [href "#users/name_d/0/25"] [text "User name ↑"]]
              DESC -> th [align "left", class "usernames"] [a [href "#users/name/0/25"] [text "User name ↓"]]
              None -> th [align "left", class "usernames"] [a [href "#users/name/0/25"] [text "User name"]]
          , case model.userOrder.posts of
              ASC  -> th [align "right"] [a [href "#users/posts_d/0/25"] [text "Post count ↑"]]
              DESC -> th [align "right"] [a [href "#users/posts/0/25"] [text "Post count ↓"]]
              None -> th [align "right"] [a [href "#users/posts/0/25"] [text "Post count"]]
          , case model.userOrder.join of
              ASC  -> th [align "right", class "dates"] [a [href "#users/joined_d/0/25"] [text "Join date ↑"]]
              DESC -> th [align "right", class "dates"] [a [href "#users/joined/0/25"] [text "Join date ↓"]]
              None -> th [align "right", class "dates"] [a [href "#users/joined/0/25"] [text "Join date"]]
          ]
        ]
      ]
    ++
      List.map viewMiniUser model.userList.users
    )
  , div [class "clearleft"] 
    [ makePNlink "Previous" model.userList.count model.userList.prevlink
    , text " "
    , makePNlink "Next" model.userList.count model.userList.nextlink
    ]
  ]

radio : String -> msg -> Bool -> Html msg
radio value msg isChecked =
  label []
    [ input [ type_ "radio", name "design-size", onClick msg, checked isChecked] []
    , text value
    ]


viewTagInfo : TagInfo -> Html Msg
viewTagInfo tag =
  tr [] 
  [ td [align "right"] [text (String.fromInt tag.count)]
  , td [align "left"] [a [href (makeUri "#tag" [tag.name, "0"])] [text tag.name]]
  ]

viewMiniUser : MiniUser -> Html Msg
viewMiniUser muser = 
  tr []
  [ td [align "left"] [a [href (makeUri "#user" [muser.name, "0"])] [text muser.name]]
  , td [align "right"] [text (String.fromInt muser.numPosts)]
  , td [align "right"] [text (makeDate muser.joinedOn)]
  ]

viewMiniList : String -> String -> String -> Model -> Html Msg
viewMiniList listType desc moreUrl model =
  case Dict.get listType model.miniLists of
    Just minilist ->
      div []
      ( [ hr [] []
        , h3 [] [text desc]
        ]
        ++
        ( let
            vcfg = makeViewConfig model Design.Mini
          in 
            List.map ((Design.view vcfg) >> (Html.map DesignMsg)) minilist
        ) ++ 
        if moreUrl /= "" then
          [ br [] []
          , a [href moreUrl] [text "view more"]
          ]
        else
          []
      )
    Nothing -> text ""

view : Model -> Browser.Document Msg
view model =
  { title = model.title
  , body =
    [ div [ id "CFAheader" ] [ p [] [text "Context Free"] ]
    , div [ id "CFAtitle" ]
      [ h1 [id "titlenode"] [text model.title]]
    , div [ id "CFAnavbar" ]
      [ ul []
        [ li [] [a [href "../index.html"] [text "Home"]]
        , li [] [a [href "index.html"] [text "Gallery"]]
        , li [] [a [href "../downloads.html"] [text "Download"]]
        , li [] [a [href "https://github.com/MtnViewJohn/context-free/wiki"] [text "Documentation"]]
        , li [] [a [href "../phpbb/index.php"] [text "Forums"]]
        ]
      ]
    , div [ id "CFAcolumn" ]
      [ h5 [] [ text "Gallery Tools:" ]
      , ul []
        [ li [] [ a [href "#newest/0"] 
                    [ case model.user of
                        LoggedIn u ->
                          if u.unseen > 0 then
                            text ("Newest (" ++ (String.fromInt u.unseen) ++ " new)")
                          else
                            text "Newest"
                        _ -> text "Newest"
                    ]
                ]
        , li [] [ a [href "#oldest/0"] [text "Oldest" ]]
        , li [] [ a [href "#title/0"]  [text "By Title" ]]
        , li [] [ a [href "#popular/0"] [text "Most Likes" ]]
        , li [] [ a [href "#random"] [text "Random" ]]
        , li [] [ a [href "#users/name/0/25"] [text "People" ]]
        , li [] [ a [href "#tags/tag"] [text "Design Tags"] ]
        , li [] [ a [href "#translate/0"] [text "Version Translator"]]
        ]
      , h5 [] [ text "Display Mode:" ]
      , ( let
            disable = model.viewMode /= Designs || (String.startsWith "#design/" model.currentHash)
          in
          ul [style "color" <| if disable then "#d8cb9f" else "inherit"]
          [ li [] 
              [ label []
                [ input
                  [ type_ "checkbox", onCheck CCcheck, checked model.limitCC, disabled disable] []
                , img 
                  [ class "top"
                  , style "padding" "0px 5px"
                  , src "graphics/CC.badge.png"
                  , alt "Creative Commons badge"
                  ] []
                , text "Only"
                ]
              ]
            , li [] [text " "]
            , li [] [ text "Display size:" ]
            , fieldset [disabled disable]
              [ li [] [radio " Small" (SwitchTo Design.Mini)  (model.designMode == Design.Mini)]
              , li [] [radio " Medium" (SwitchTo Design.Small) (model.designMode == Design.Small)]
              , li [] [radio " Large" (SwitchTo Design.Medium) (model.designMode == Design.Medium)]
              ]
            ])
      , h5 [] [ text "Lookup" ]
      , ul []
        [ li [] 
          [ Html.form [ onSubmit LookupName ]
            [ fieldset []
              [ label []
                [ text "Author "
                , input [ type_ "text", name "by", size 8, placeholder "name", id "lookname"
                        , value model.authorLookup, onInput AuthorText
                        , attribute "data-lpignore" "true"
                        , style "padding" "1px"] []
                , input [ type_ "submit", value "Go", style "padding" "1px"] [ ]
                ]
              ]
            ]
          ]
        , li []
          [ Html.form [ onSubmit LookupDesign ]
            [ fieldset []
              [ label []
                [ text "Design "
                , input [ type_ "text", name "id", size 8, placeholder "id #", id "lookid"
                        , value (designString model.designLookup), onInput DesignText
                        , attribute "data-lpignore" "true"
                        , style "padding" "1px"] []
                , input [ type_ "submit", value "Go", style "padding" "1px"] [ ]
                ]
              ]
            ]
          ]
        ]
      , case model.user of
          LoggedIn user ->
            div []
            [ h5 [] [ text ("User " ++ user.name) ]
            , ul []
              [ li [] [ a [href "#edit/0"] [text "Upload a Design" ]]
              , li [] [ a [href (makeUri "#user" [user.name, "0"])] [text "My uploads" ]]
              , li [] [ a [ onClick LogoutClick, href "#" ] [ text "Logout" ]]
              ]
            , div [hidden True]
              [ Html.map LoginMsg (Login.view model.loginform "") ]
            ]
          LoginPending ->
            div [hidden True] 
            [ Html.map LoginMsg (Login.view model.loginform "")]
          NotLoggedIn errmsg ->
            div [] [ Html.map LoginMsg (Login.view model.loginform errmsg)]
      ]
    , div [ id "CFAcontent" ]
      ( case model.viewMode of
          Error ->
            [ div [ property "innerHTML" (JE.string model.errorMessage) ] []
            ]
          Editing ->
            case model.editDesign of
              Nothing -> [makeDownBar True zeroList]
              Just edesign ->
                [ Html.map EDesignMsg (Design.viewEdit model.tagList edesign)
                , if model.errorMessage == "" then
                    text ""
                  else
                    div [class "alertbox"] [text model.errorMessage]
                ]
          EditingTags ->
            case model.editDesign of
              Nothing -> [makeDownBar True zeroList]
              Just edesign ->
                [ Html.map EDesignMsg (Design.viewEditTags model.tagList edesign)
                , if model.errorMessage == "" then
                    text ""
                  else
                    div [class "alertbox"] [text model.errorMessage]
                ]
          Designs ->
            viewDesigns model
          Tags ->
            [ table [class "tagstable"]
              ( [ thead [] 
                  [ tr [] 
                    [ th [align "right"] [a [href "#tags/count"] [text "Count"]]
                    , th [align "left"] [a [href "#tags/tag"] [text "Tag"]]
                    ]
                  ]
                ]
              ++
                List.map viewTagInfo model.tagList
              )
            ]
          People ->
            viewUsers model
          Translation ->
            [ if model.errorMessage == "" then
                    text ""
                  else
                    div [class "alertbox"] [text model.errorMessage]
            , h1 [] [text "Cfdg text translated from v2 syntax to v3:"]
            , textarea 
              [ id "cfdg3txt"
              , class "cfdg"
              , rows 100
              , cols 100
              , readonly True
              , value model.cfdg3text] []
            , button [ class "copy-button", attribute "data-clipboard-target" "#cfdg3txt" ]
                     [ text "Copy to clipboard" ]
            , h1 [] [text "Enter some v2 cfdg text to translate:"]
            , Html.form [onSubmit TranslateText]
              [ textarea [class "cfdg", rows 100, cols 100, value model.cfdg2text, onInput Cfdg2Change] []
              , h2 [] [text "Or select (or drop) a cfdg file"]
              , input [type_ "file", name "cfdgfile", id "cfdgfile"
                      , on "change" (JD.map TranslateFile decodeFiles)][]
              , p [] 
                [ text "Note: Targeted color changes are not translated. They cannot be automatically translated and require manual fixup. "
                , a [href "https://github.com/MtnViewJohn/context-free/wiki/Version-2-Syntax#targeted-color-adjustments"] [text "This page"]
                , text " describes why."
                ]
              , input [type_ "submit", name "submit", value "Translate!"] []
              ]
            ]
          Default ->
            [ table []
              [ tr []
                [ td [class "vupload"]
                  [ text """
       Context Free/cfdg is a simple language for generating stunning images.
       With only a few lines you can describe abstract art, beautiful organic scenery,
       and many kinds of fractals. It's highly addictive!
                    """
                  , br [] []
                  , br [] []
                  , text "The Context Free Gallery is a public repository for artwork made "
                  , text "using the language."
                  ]
                , td [class "vupload"]
                  [ h2 [] [text "What next?"]
                  , table []
                    [ tr []
                      [ td []
                        [ a [ href "../downloads.html", class "call-to-action"] 
                            [ text "Get the software"]]
                      , td []
                        [ a [ href "https://github.com/MtnViewJohn/context-free/wiki"
                            , class "call-to-action"
                            ]
                            [ text "Learn Context Free"]]
                      ]
                    , tr []
                      [ td []
                        [ a [ href "../phpbb/ucp.php?mode=register", class "call-to-action"]
                            [ text " Join the gallery "]]
                      , td []
                        [ a [ href "#newest/0", class "call-to-action"] 
                            [ text "See what people are doing"]]
                      ]
                    ]
                  ]
                ]
              ]
            , viewMiniList "newest" "Newest designs:" "#newest/0" model
            , viewMiniList "newbie" "Newest contributors:" "" model
            , viewMiniList "popula" "Some popular designs:" "#popular/0" model
            , viewMiniList "random" "Some random designs:" 
                ("#random/" ++ (String.fromInt model.miniSeed) ++ "/0") model
            ]
      )
    , div [ id "CFAfooter" ] []
    , div [ id "CFAbook" ]
      [ a [href "../book.html"]
        [ img [src "graphics/cover-shot-120.png", alt "book cover"] []
        , span [class "CFAmessage"]
          [ span [class "CFAleadin"] [text "See our book:"]
          , br [] []
          , span [class "CFAtitle"] [text "Community of Variation"]
          ]
        ]
      ]
    ]
  }



-- HTTP

maybeRiskyRequest : String -> (
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
  -> Cmd msg)
maybeRiskyRequest backend =
  if String.contains "localhost" backend then
    Http.riskyRequest
  else
    Http.request


myExpectJson : (Result HttpError18 a -> msg) -> JD.Decoder a -> Http.Expect msg
myExpectJson toMsg decoder =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url          -> Err (BadUrl url)
        Http.Timeout_             -> Err Timeout
        Http.NetworkError_        -> Err NetworkError
        Http.BadStatus_ meta body -> Err (BadStatus meta.statusCode body)
        Http.GoodStatus_ _ body   ->
          case JD.decodeString decoder body of
            Ok value ->
              Ok value
            Err err ->
              Err (BadPayload (JD.errorToString err) 500 body)

resolveAction : Maybe Action -> Model -> Cmd Msg
resolveAction ma model =
  case ma of 
    Nothing -> Cmd.none
    Just act -> case act of
      UpdateComment commentid comment_ -> sendComment commentid comment_ model
      CreateComment comment_ -> newComment comment_ model
      AddFaves designid -> changeFave "addfave" designid model
      RemoveFaves designid -> changeFave "deletefave" designid model
      CancelEditAct -> Nav.back model.key 1
      UploadDesign -> case model.editDesign of
        Nothing -> Cmd.none
        Just edesign -> uploadDesign edesign model
      UploadDesignTags -> case model.editDesign of
        Nothing -> Cmd.none
        Just edesign -> uploadDesignTags edesign model
      _ -> Cmd.none

updateDesignStatus : Model -> Cmd Msg
updateDesignStatus model =
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/newdesigns"
    , body = Http.emptyBody
    , expect = Http.expectJson DesignStatusUpdated decodeStatus
    , timeout = Nothing
    , tracker = Nothing
    }

checkUnseen : Model -> Cmd Msg
checkUnseen model =
  case model.user of
    LoggedIn u ->
      maybeRiskyRequest model.backend
        { method = "GET"
        , headers = []
        , url = model.backend ++ "/unseen"
        , body = Http.emptyBody
        , expect = Http.expectJson ReceiveUnseen decodeUnseen
        , timeout = Nothing
        , tracker = Nothing
        }
    _ -> Cmd.none

decodeUnseen : JD.Decoder Int
decodeUnseen =
  JD.field "unseen" JD.int


decodeStatus : JD.Decoder Bool
decodeStatus =
  JD.field "newdesigns" <| JD.map (\i -> i /= 0) JD.int

changeFave : String -> DesignID -> Model -> Cmd Msg
changeFave change designid model =
  maybeRiskyRequest model.backend 
    { method = "POST"
    , headers = []
    , url = String.join "/" [model.backend, change, idStr designid]
    , body = Http.emptyBody
    , expect = Http.expectJson NewFaves decodeFaves
    , timeout = Nothing
    , tracker = Nothing
    }

decodeFaves : JD.Decoder FaveInfo
decodeFaves =
  JD.map2 FaveInfo
    (JD.field "designid"  <| JD.map ID JD.int)
    (JD.field "faves" (JD.list JD.string))

sendComment : CommentID -> String -> Model -> Cmd Msg
sendComment commentid comment_ model =
  maybeRiskyRequest model.backend
    { method = "PUT"
    , headers = []
    , url = model.backend ++ "/updatecomment/" ++ (cidStr commentid)
    , body = Http.stringBody "text/plain" comment_
    , expect = Http.expectJson NewComment Comment.decodeComment
    , timeout = Nothing
    , tracker = Nothing
    }

newComment : String -> Model -> Cmd Msg
newComment comment_ model =
  maybeRiskyRequest model.backend
    { method = "PUT"
    , headers = []
    , url = model.backend ++ "/createcomment/" ++ (idStr model.mainDesign)
    , body = Http.stringBody "text/plain" comment_
    , expect = Http.expectJson NewComment Comment.decodeComment
    , timeout = Nothing
    , tracker = Nothing
    }

deleteComment : CommentID -> Model -> Cmd Msg
deleteComment commentid model =
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/deletecomment/" ++ (cidStr commentid)
    , body = Http.emptyBody
    , expect = Http.expectJson RemoveComment decodeCommentId
    , timeout = Nothing
    , tracker = Nothing
    }

decodeCommentId : JD.Decoder CommentID
decodeCommentId =
  JD.field "commentid" (JD.map CID JD.int)
      

getDesign : DesignID -> Model -> Cmd Msg
getDesign id model =
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/design/" ++ (idStr id)
    , body = Http.emptyBody
    , expect = Http.expectJson NewDesign decodeDesignTag
    , timeout = Nothing
    , tracker = Nothing
    }

decodeDesignTag : JD.Decoder DesignTags
decodeDesignTag =
  JD.map2 DesignTags
    (JD.field "design" Design.decodeDDesign)
    (JD.maybe <| JD.field "tags" (JD.list decodeTagInfo))

loadEditDesign : DesignID -> Model -> Cmd Msg
loadEditDesign id model =
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/design/" ++ (idStr id)
    , body = Http.emptyBody
    , expect = Http.expectJson NewEditDesign decodeEDesign
    , timeout = Nothing
    , tracker = Nothing
    }

decodeEDesign : JD.Decoder Design.EditDesign
decodeEDesign =
   JD.field "design" Design.decodeEDesign

uploadDesign : Design.EditDesign -> Model -> Cmd Msg
uploadDesign edesign model =
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/fpostdesign"
    , body = Http.multipartBody <| Design.encodeDesign edesign
    , expect = myExpectJson UploadResponse decodeDesignTag
    , timeout = Nothing
    , tracker = Nothing
    }

uploadDesignTags : Design.EditDesign -> Model -> Cmd Msg
uploadDesignTags edesign model =
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/fpostdesigntags"
    , body = Http.multipartBody <| Design.encodeDesign edesign
    , expect = myExpectJson UploadResponse decodeDesignTag
    , timeout = Nothing
    , tracker = Nothing
    }

getDesigns : Model -> String -> Int -> Int -> Cmd Msg
getDesigns model query start count =
  let
    ccquery = if model.limitCC then "cc" ++ query else query
    url = String.join "/" [model.backend, ccquery, 
                           String.fromInt(start), String.fromInt(count)]
  in
    maybeRiskyRequest model.backend
      { method = "GET"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson NewDesigns decodeDesigns
      , timeout = Nothing
      , tracker = Nothing
      }

decodeDesigns : JD.Decoder DesignList
decodeDesigns = 
  JD.map7 DesignList
    (JD.field "designs" (JD.array Design.decodeDDesign))
    (JD.field "prevlink" JD.string)
    (JD.field "nextlink" JD.string)
    (JD.field "thislink" JD.string)
    (JD.field "start"    JD.int)
    (JD.field "count"    JD.int)
    (JD.succeed "")

getMiniList : Model -> String -> Int -> Cmd Msg
getMiniList model listType count =
  let
    url = String.join "/" [model.backend, listType, "0", String.fromInt(count)]
  in
    maybeRiskyRequest model.backend
      { method = "GET"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson (NewMiniList <| String.left 6 listType) decodeMiniList
      , timeout = Nothing
      , tracker = Nothing
      }

decodeMiniList : JD.Decoder (List Design.DisplayDesign)
decodeMiniList =
  JD.field "designs" (JD.list Design.decodeDDesign)

deleteDesign : DesignID -> Model -> Cmd Msg
deleteDesign designid model =
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/delete/" ++ (idStr designid)
    , body = Http.emptyBody
    , expect = Http.expectJson DeleteADesign decodeDesignId
    , timeout = Nothing
    , tracker = Nothing
    }

decodeDesignId : JD.Decoder DesignID
decodeDesignId = 
  JD.field "designid" <| JD.map ID JD.int


loginUser : Model -> Cmd Msg
loginUser model =
  let
    url = makeUri 
      model.backend
      [ "login"
      , model.loginform.user
      , model.loginform.password
      , if model.loginform.remember then "1" else "0"
      ]
  in
    maybeRiskyRequest model.backend
      { method = "POST"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson NewUser decodeUser
      , timeout = Nothing
      , tracker = Nothing
      }

loginSession : String -> Cmd Msg
loginSession backend =
  maybeRiskyRequest backend
    { method = "GET"
    , headers = []
    , url = backend ++ "/userinfo"
    , body = Http.emptyBody
    , expect = Http.expectJson SessionUser decodeUser
    , timeout = Nothing
    , tracker = Nothing
    }

decodeUser : JD.Decoder User.User
decodeUser =
  JD.field "userinfo" User.decodeUser

logoutUser : Model -> Cmd Msg
logoutUser model = 
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/logout"
    , body = Http.emptyBody
    , expect = Http.expectJson LogoutUser decodeUserLogout
    , timeout = Nothing
    , tracker = Nothing
    }

decodeUserLogout : JD.Decoder Bool
decodeUserLogout =
  JD.field "logout_success" JD.bool

getCfdg : DesignID -> Model -> Cmd Msg
getCfdg id model =
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/data/cfdg/" ++ (idStr id)
    , body = Http.emptyBody
    , expect = Http.expectString <| ReceiveCfdg id
    , timeout = Nothing
    , tracker = Nothing
    }

getCfdgfromDesign : Model -> Design.DisplayDesign -> Cmd Msg
getCfdgfromDesign model ddesign =
  getCfdg ddesign.design.designid model

getInfo : DesignID -> Model -> Cmd Msg
getInfo id model =
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/auxinfo/" ++ (idStr id)
    , body = Http.emptyBody
    , expect = Http.expectJson (ReceiveInfo id) decodeInfo
    , timeout = Nothing
    , tracker = Nothing
    }

decodeInfo : JD.Decoder DesignInfo
decodeInfo =
  JD.map3 DesignInfo
    (JD.field "fans" (JD.list JD.string))
    (JD.field "tags" (JD.list JD.string))
    (JD.maybe <| JD.field "imagesize" Design.decodeSize)

getComments : DesignID -> Model -> Cmd Msg
getComments id model =
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/comments/" ++ (idStr id)
    , body = Http.emptyBody
    , expect = Http.expectJson NewComments decodeComments
    , timeout = Nothing
    , tracker = Nothing
    }

decodeComments : JD.Decoder CommentList
decodeComments =
  JD.map2 CommentList
    (JD.field "comments" (JD.list Comment.decodeComment))
    (JD.field "designid" (JD.map ID JD.int))

getTitle : Model -> String -> Cmd Msg
getTitle model title =
  let
    url = 
      if model.limitCC then
        model.backend ++ "/cctitleindex/" ++ (Url.percentEncode title)
      else
        model.backend ++ "/titleindex/" ++ (Url.percentEncode title)
  in
    maybeRiskyRequest model.backend
      { method = "GET"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson GotTitleIndex decodeTitleIndex
      , timeout = Nothing
      , tracker = Nothing
      }

decodeTitleIndex : JD.Decoder Int
decodeTitleIndex = 
  JD.field "index" JD.int

getTags : Model -> Cmd Msg
getTags model = 
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/tags"
    , body = Http.emptyBody
    , expect = Http.expectJson GotTags decodeTags
    , timeout = Nothing
    , tracker = Nothing
    }

decodeTags : JD.Decoder (List TagInfo)
decodeTags =
  JD.field "tags" (JD.list decodeTagInfo)

decodeTagInfo : JD.Decoder TagInfo
decodeTagInfo =
  JD.map2 TagInfo
    (JD.field "name"  JD.string)
    (JD.field "count" JD.int)

      
getUsers: String -> Int -> Int -> Model -> Cmd Msg
getUsers query start count model =
  let
    url = String.join "/" [model.backend, query, 
                           String.fromInt(start), String.fromInt(count)]
  in
    maybeRiskyRequest model.backend
      { method = "GET"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson NewUsers decodeUsers
      , timeout = Nothing
      , tracker = Nothing
      }

decodeUsers : JD.Decoder UserList
decodeUsers =
  JD.map5 UserList
    (JD.field "users"    (JD.list User.decodeMiniUser))
    (JD.field "prevlink"  JD.string)
    (JD.field "nextlink"  JD.string)
    (JD.field "thislink"  JD.string)
    (JD.field "count"     JD.int)

translateDesign : DesignID -> Model -> Cmd Msg
translateDesign id model =
  maybeRiskyRequest model.backend
    { method = "GET"
    , headers = []
    , url = model.backend ++ "/translate/" ++ (idStr id)
    , body = Http.emptyBody
    , expect = myExpectJson NewCfdg3 decodeCfdg3
    , timeout = Nothing
    , tracker = Nothing
    }

translateText : Model -> Cmd Msg
translateText model =
  maybeRiskyRequest model.backend
    { method = "POST"
    , headers = []
    , url = model.backend ++ "/translate"
    , body = Http.stringBody "text/plain" model.cfdg2text
    , expect = myExpectJson NewCfdg3 decodeCfdg3
    , timeout = Nothing
    , tracker = Nothing
    }


decodeCfdg3 : JD.Decoder Cfdg3Info
decodeCfdg3 =
  JD.map3 Cfdg3Info
    (JD.field "cfdg3txt"    JD.string)
    (JD.field "colortarget" JD.bool)
    (JD.field "discards"    JD.bool)
      
      

-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch 
  ( scrolledToElement TryScroll ::
    if model.viewMode == Designs && model.designList.nextlink /= "" then
      [ Time.every 250.0 TickTock
      , isVisible IsVisible
      ]
    else
      []
  )
  
