import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onSubmit, onInput, onWithOptions)
import String exposing (trimLeft, toInt)
import Login
import Design
import Comment
import User exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Http
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Random
import GalleryUtils exposing (..)
import Time exposing (Time)
import Task
import Array exposing (Array)
import Ports exposing (FilePortData, fileSelected, fileContentRead)

main : Program Flags Model Msg
main =
  Navigation.programWithFlags
    NewURL
    { init = initModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type ViewMode
  = Designs
  | Tags
  | People
  | Translation
  | Default
  | Editing
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

type alias FaveInfo =
  { designid : Int
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


type alias Model =
  { user : Maybe User
  , loginform : Login.Model
  , limitCC : Bool
  , authorLookup : String
  , designLookup : Int
  , viewMode : ViewMode
  , mainDesign : Int
  , designList : DesignList
  , pendingLoad : Bool
  , editDesign : Maybe Design.EditDesign
  , designMode : Design.ViewSize
  , tagList : List TagInfo
  , userList : UserList
  , userOrder : UserOrder
  , errorMessage : String
  , initUrl : String
  , currentHash : String
  , errorInfo : Result Http.Error String
  , backend : String
  }

zeroList : DesignList
zeroList = DesignList Array.empty "" "" "" 0 0 ""

zeroUList : UserList
zeroUList = UserList [] "" "" "" 0

initModel : Flags -> Navigation.Location -> (Model, Cmd Msg)
initModel flags loc = 
  let
    model = Model Nothing Login.initModel False "" 0 Designs noDesign zeroList False Nothing 
            Design.Small [] zeroUList (UserOrder ASC None None) "" loc.href "" (Ok "") flags.backend
  in
    (model, loginSession model)

designMap : (Design.DisplayDesign -> Design.DisplayDesign) -> DesignList -> DesignList
designMap mapping oldList =
  { oldList | designs = Array.map mapping oldList.designs }

noDesign : Int
noDesign = -1

designFind : Int -> (Array Design.DisplayDesign) -> (Int, Maybe Design.DisplayDesign)
designFind id darray =
  let
    find2 = \index ->
      if index < 0 then
        (noDesign, Nothing)
      else case Array.get index darray of
        Nothing -> (noDesign, Nothing)
        Just design ->
          if design.design.designid == id then
            (index, Just design)
          else
            find2 (index - 1)
  in
    find2 ((Array.length darray) - 1)
      
      
dmerge : String -> DesignList -> DesignList -> DesignList
dmerge hash new old =
  let
    newparts = String.split "/" new.thislink
    oldparts = String.split "/" old.thislink
    name_ = String.dropLeft 1 hash
  in case (List.head newparts, List.head oldparts) of
    (Just newhead, Just oldhead) -> 
      if (newhead /= oldhead) then
        {new | currentHash = hash}
      else if old.start + old.count == new.start then
        let
          mfirst = Array.get 0 new.designs
          ndesigns_ = case mfirst of
            Nothing -> new.designs
            Just ddesign ->
              Array.set 0 {ddesign | name = name_} new.designs
        in
          {old | designs = Array.append old.designs ndesigns_
               , nextlink = new.nextlink
               , count = old.count + new.count
               , currentHash = hash}
      else if new.start + new.count == old.start then
        let
          mfirst = Array.get 0 old.designs
          odesigns_ = case mfirst of
            Nothing -> old.designs
            Just ddesign ->
              Array.set 0 {ddesign | name = name_} old.designs
        in
          {old | designs = Array.append new.designs odesigns_
               , prevlink = new.prevlink
               , count = old.count + new.count
               , thislink = new.thislink
               , currentHash = hash}
      else
        {new | currentHash = hash}
    _ -> {new | currentHash = hash}
      

-- URL PARSING


type Route
  = Home
  | Login String String String
  | ErrorMsg String
  | DesignID Int
  | EditDesign Int
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





route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map Home top
    , Url.map Login (Url.s "login" </> Url.string </> Url.string </> Url.string)
    , Url.map ErrorMsg (Url.s "error" </> Url.string)
    , Url.map DesignID (Url.s "design" </> int)
    , Url.map EditDesign (Url.s "edit" </> int)
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
    ]




-- UPDATE

type Msg
  = LogoutClick
  | CCcheck Bool
  | SwitchTo Design.ViewSize
  | NewURL Navigation.Location
  | LoadDesigns String
  | LoginMsg Login.Msg
  | DesignMsg Design.MsgId
  | ClearDeleteBut Int
  | EDesignMsg Design.EMsg
  | LookupName
  | LookupDesign
  | AuthorText String
  | DesignText String
  | NewSeed Int
  | DismissDesign
  | LoadDesign Time
  | NewDesign (Result Http.Error DesignTags)
  | NewEditDesign (Result Http.Error Design.EditDesign)
  | NewDesigns (Result Http.Error DesignList)
  | NewUser (Result Http.Error User.User)
  | SessionUser (Result Http.Error User.User)
  | LogoutUser (Result Http.Error Bool)
  | ReceiveCfdg Int (Result Http.Error String)
  | ReceiveInfo Int (Result Http.Error DesignInfo)
  | NewComments (Result Http.Error (List Comment.Comment))
  | NewComment (Result Http.Error Comment.Comment)
  | RemoveComment (Result Http.Error Int)
  | GotTitleIndex (Result Http.Error Int)
  | GotTags (Result Http.Error (List TagInfo))
  | NewUsers (Result Http.Error UserList)
  | NewFaves (Result Http.Error FaveInfo)
  | DeleteADesign (Result Http.Error Int)
  | UploadResponse (Result Http.Error DesignTags)
  | FileRead FilePortData

updateDesigns : Model -> String -> Int -> Int -> (Model, Cmd Msg)
updateDesigns model query start count =
  let
    go2server = model.currentHash /= model.designList.currentHash ||
                model.currentHash == ""
  in
    ({model | mainDesign = noDesign, viewMode = Designs, pendingLoad = go2server},
      if go2server then
        getDesigns model query start count
      else
        Cmd.none)

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
        (model_, Navigation.newUrl url_)
    SwitchTo size ->
      let
        model_ = {model | designMode = size}
        url_ = (makeQuery model_) ++ "#" ++ model_.designList.thislink
      in
        (model_, Navigation.newUrl url_)
    LoadDesigns newHash ->
      (model, Navigation.modifyUrl ("#" ++ newHash))
    NewURL loc ->
      let
        qbits = String.split "&" (String.dropLeft 1 loc.search)
        cc_ = List.member "cc" qbits
        mode_ = if (List.member "large" qbits) then Design.Medium else Design.Small
        model_ = {model | limitCC = cc_
                        , designMode = mode_
                        , currentHash = loc.hash}
        dcount = if model_.designMode == Design.Small then 50 else 5
      in
        case Url.parseHash route loc of
          Nothing ->
            (model_, Cmd.none)
          Just route ->
            case route of
              Home ->
                let
                  model__ = {model_ | viewMode = Default
                                    , mainDesign = noDesign
                                    , designList = zeroList
                                    , pendingLoad = True}
                in
                  (model__, Cmd.batch [getDesigns model__ "newest" 0 10, getNewbie model__])
              Login user password remember ->
                let
                  err = case (user, password) of
                    ("", _) -> "Username required"
                    (_, "") -> "Password required"
                    _ -> ""
                  login_ = Login.Model user password (remember /= "0") err
                  model__ = {model_ | loginform = login_}
                in
                  (model__, 
                    if err == "" then
                      loginUser model__
                    else
                      Cmd.none)
              ErrorMsg msg_enc ->
                let
                  msg = Maybe.withDefault "Malformed error message." (Http.decodeUri msg_enc)
                in
                  ({model_ | errorMessage = msg, viewMode = Error}, Cmd.none)
              DesignID id ->
                let
                  (index, mddesign) = designFind id model_.designList.designs
                in case mddesign of
                  Nothing -> 
                    ({model_  | mainDesign = noDesign
                              , viewMode = Designs
                              , designList = zeroList
                              , pendingLoad = True },
                      getDesign id model_)
                  Just ddesign ->
                    ({model_  | mainDesign = index
                              , viewMode = Designs},
                      Cmd.batch 
                        [ getComments ddesign.design.designid model_
                        , getCfdg ddesign.design.designid model_
                        , getInfo ddesign.design.designid model_
                        ])
              EditDesign id ->
                case model_.user of
                  Nothing -> (model_, Cmd.none)
                  Just user ->
                    if id == 0 then
                      ({model_ | editDesign = Nothing, viewMode = Editing, errorMessage = ""}
                      , Task.perform LoadDesign Time.now)
                    else
                      let
                        (_, mddesign) = designFind id model_.designList.designs
                      in case mddesign of
                        Nothing ->
                          ({model_  | editDesign = Nothing
                                    , viewMode = Editing
                                    , errorMessage = ""
                                    , designList = zeroList
                                    , pendingLoad = True},
                            loadEditDesign id model_)
                        Just ddesign ->
                          ({model_ | editDesign = Just <| Design.makeEDesign ddesign.design
                                   , viewMode = Editing
                                   , errorMessage = ""}, Cmd.none)
              Author name_enc start count ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  updateDesigns model_ (makeUri "by" [name]) start count
              AuthorInit name_enc start ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  ({model_  | designList = zeroList },
                    Navigation.modifyUrl (makeUri "#user" [name, toString start, toString dcount]))
              AuthorInit2 name_enc ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  (model_, Navigation.modifyUrl (makeUri "#user" [name, "0", toString dcount]))
              Faves name_enc start count ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  updateDesigns model_ (makeUri "faves" [name]) start count
              FavesInit name_enc start ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  ({model_  | designList = zeroList },
                    Navigation.modifyUrl (makeUri "#faves" [name, toString start, toString dcount]))
              Newest start count ->
                updateDesigns model_ "newest" start count
              NewestInit start ->
                ({model_  | designList = zeroList },
                  Navigation.modifyUrl (makeUri "#newest" [toString start, toString dcount]))
              Oldest start count ->
                updateDesigns model_ "oldest" start count
              OldestInit start ->
                ({model_  | designList = zeroList },
                  Navigation.modifyUrl (makeUri "#oldest" [toString start, toString dcount]))
              Title start count ->
                updateDesigns model_ "title" start count
              TitleInit start ->
                ({model_  | designList = zeroList },
                  Navigation.modifyUrl (makeUri "#title" [toString start, toString dcount]))
              TitleIndex title ->
                (model_, getTitle model_ title)
              Popular start count ->
                updateDesigns model_ "popular" start count
              PopularInit start ->
                ({model_  | designList = zeroList },
                  Navigation.modifyUrl (makeUri "#popular" ["0", toString dcount]))
              RandomDes seed start count ->
                updateDesigns model_ ("random/" ++ (toString seed)) start count
              RandomInit seed start ->
                ({model_  | designList = zeroList },
                  Navigation.modifyUrl (makeUri "#random" [toString seed, "0", toString dcount]))
              RandomSeed ->
                (model_, Random.generate NewSeed (Random.int 1 1000000000))
              Tag tag_enc start count ->
                let
                  tag = Maybe.withDefault "" (Http.decodeUri tag_enc)
                in
                  updateDesigns model_ (makeUri "tag" [tag]) start count
              TagInit tag_enc start ->
                let
                  tag = Maybe.withDefault "" (Http.decodeUri tag_enc)
                in
                  ({model_ | designList = zeroList },
                    Navigation.modifyUrl (makeUri "#tag" [tag, "0", toString dcount]))
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
                  ({model_| tagList = List.sortWith comp model_.tagList, viewMode = Tags}, Cmd.none)
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
                    ({model_ | viewMode = People, userOrder = userOrder_}, 
                      getUsers ("users/" ++ utype) start count model_)
                  else
                    (model_, Cmd.none)
    NewSeed seed ->
      (model, Navigation.newUrl ("#random/" ++ (toString seed ++ "/0")))
    LoginMsg lMsg ->
      let
        newLoginModel = Login.update lMsg model.loginform          
      in
        ({ model | loginform = newLoginModel }, Cmd.none) 
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
              Just (ClearDelete id) -> update (ClearDeleteBut id) model_
              _ -> (model_, resolveAction act_ model_)
    ClearDeleteBut id ->
      let
        designList = model.designList
        designs_ = Array.map (Design.clearDeleteBut id) designList.designs
        designList_ = { designList | designs = designs_ }
      in
        ({model | designList = designList_}, Cmd.none)
    EDesignMsg emsg -> case model.editDesign of
      Nothing -> (model, Cmd.none)
      Just edesign ->
        let
          (edesign_, act_) = Design.editupdate emsg edesign
          model_ = {model | editDesign = Just edesign_}
        in
          (model_, resolveAction act_ model_)
    LookupName ->
      (model, Navigation.newUrl (makeUri "#user" [model.authorLookup, "0"]))
    LookupDesign ->
      (model, Navigation.newUrl ("#design/" ++ (toString model.designLookup)))
    AuthorText author ->
      ({ model | authorLookup = trimLeft author }, Cmd.none)
    DesignText desText ->
      let
        des = Result.withDefault 0 (String.toInt desText)
      in
        if des < 1  && desText /= "" then
          (model, Cmd.none)
        else
          ({ model | designLookup = des }, Cmd.none)
    DismissDesign ->
      ({model | mainDesign = noDesign}, Navigation.back 1)
    LoadDesign time ->
      case model.user of
        Nothing -> (model, Cmd.none)
        Just user ->
          let
            edesign_ = Design.initDesign user.name user.defaultccURI time
          in
            ({model | editDesign = Just edesign_}, Cmd.none)
    NewDesign designResult ->
      case designResult of
        Ok dt ->
          let
            design = dt.design
            tags_ = Maybe.withDefault model.tagList dt.tags
            designList_ = DesignList (Array.fromList [design]) "" "" "" 0 1 ""
          in
            ({model | designList = designList_
                    , tagList = tags_
                    , mainDesign = 0
                    , pendingLoad = False
                    , errorInfo = Ok "New design"},
              Cmd.batch 
                [ getComments design.design.designid model
                , getCfdg design.design.designid model
                ])
        Err error ->
          ({ model | mainDesign = noDesign, errorInfo = Err error}, Cmd.none)
    NewEditDesign designResult ->
      case designResult of
        Ok design ->
          ({model | editDesign = Just design
                  , pendingLoad = False
                  , errorInfo = Ok "New edit design"}, Cmd.none)
        Err error ->
          ({ model | editDesign = Nothing, errorInfo = Err error}, Cmd.none)
    NewDesigns designResult ->
      case designResult of
        Ok designs ->
          ({model | designList = dmerge model.currentHash designs model.designList
                  , mainDesign = noDesign
                  , pendingLoad = False
                  , errorInfo = Ok "New designs"},         
            if model.designMode == Design.Small then
              Cmd.none
            else
              Cmd.batch (List.map (getCfdgfromDesign model) (Array.toList designs.designs)))
        Err error ->
          ({model | designList = zeroList, errorInfo = Err error, pendingLoad = False}, Cmd.none)
    NewUser loginResult ->
      case loginResult of
        Ok user ->
          ({model | user = Just user, errorInfo = Ok "Login success"}, 
            Navigation.modifyUrl "#newest/0")
        Err error ->
          let
            loginform_ = Login.fail "Login failed" model.loginform          
          in
            ({ model | loginform = loginform_, errorInfo = Err error }, Cmd.none) 
    SessionUser loginResult ->
      case loginResult of
        Ok user ->
          ({model | user = Just user, errorInfo = Ok "Session loaded"}, getTags model)
        Err error ->
          ({model | errorInfo = Err error}, getTags model)
    LogoutUser logoutResult ->
      case logoutResult of
        Ok yes ->
          ({model | user = Nothing, errorInfo = Ok "Logout success"}, Cmd.none)
        Err error ->
          let
            newLoginModel = Login.fail "Logout failed" model.loginform          
          in
            ({ model | loginform = newLoginModel, errorInfo = Err error}, Cmd.none) 
    ReceiveCfdg id cfdgResult ->
      case cfdgResult of
        Ok cfdgText ->
          let
            (index, mddesign) = designFind id model.designList.designs
          in case mddesign of
            Nothing -> (model, Cmd.none)  -- Shouldn't happen
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
          in case mddesign of
            Nothing -> (model, Cmd.none)  -- Shouldn't happen
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
                ({model | designList = designList_, errorInfo = Ok "Info received"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    NewComments commentResult ->
      case commentResult of
        Ok comments ->
          let
            index = model.mainDesign
          in case Array.get index model.designList.designs of
            Nothing -> (model, Cmd.none)  -- Shouldn't happpen
            Just ddesign ->
              let
                ddesign_ = Design.setComments comments ddesign
                designList = model.designList
                designList_ = {designList | designs = Array.set index ddesign_ designList.designs}
              in
                ({model | designList = designList_, errorInfo = Ok "Comments freceived"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    NewComment commentResult ->
      case commentResult of
        Ok comment -> case Array.get model.mainDesign model.designList.designs of
          Nothing -> (model, Cmd.none)  -- Shouldn't happpen
          Just ddesign ->
            let
              ddesign_ = Design.setComment comment ddesign
              designList = model.designList
              designList_ = {designList | designs = Array.set model.mainDesign ddesign_ designList.designs}
            in
              ({model | designList = designList_, errorInfo = Ok "Comment received"}, Cmd.none)
        Err error -> ({model | errorInfo = Err error}, Cmd.none)
    RemoveComment commentidResult ->
      case commentidResult of
        Ok commentid -> case Array.get model.mainDesign model.designList.designs of
          Nothing -> (model, Cmd.none)  -- Shouldn't happpen
          Just ddesign ->
            let
              ddesign_ = Design.removeComment commentid ddesign
              designList = model.designList
              designList_ = {designList | designs = Array.set model.mainDesign ddesign_ designList.designs}
            in
              ({model | designList = designList_, errorInfo = Ok "Comment deleted"}, Cmd.none)
        Err error -> ({model | errorInfo = Err error}, Cmd.none)
    GotTitleIndex indexResult ->
      case indexResult of
        Ok index ->
          ({ model | errorInfo = Ok "Title index"}, Navigation.newUrl ("#title/" ++ (toString index)))
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    GotTags tagsResult ->
      let
        cmd = if model.initUrl == "" then
          Cmd.none
        else
          Navigation.modifyUrl model.initUrl
      in
        case tagsResult of
          Ok tags ->
            ({ model | tagList = tags
                     , errorInfo = Ok "Tags"
                     , initUrl = ""}, cmd)
          Err error ->
              ({model | errorInfo = Err error
                      , initUrl = ""}, cmd)
    NewUsers usersResult ->
      case usersResult of
        Ok users ->
          ({model | userList = users, errorInfo = Ok "User list"}, Cmd.none)
        Err error ->
          ({model | errorInfo = Err error}, Cmd.none)
    NewFaves favesResults -> case favesResults of
      Ok faveinfo -> case Array.get model.mainDesign model.designList.designs of
        Just ddesign ->
          if ddesign.design.designid == faveinfo.designid then
            let
              design = ddesign.design
              design_ = {design
                         | fans = faveinfo.fans
                         , numvotes = List.length faveinfo.fans}
              ddesign_ = {ddesign | design = design_}
              designList = model.designList
              designList_ = {designList | designs = Array.set model.mainDesign ddesign_ model.designList.designs}
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
                Navigation.newUrl "#"
              else
                Navigation.newUrl ("#" ++ designList_.thislink)
          in
            ({model | designList = designList_, mainDesign = noDesign
                    , errorInfo = Ok "Design deleted"}, cmd_)
        Err error ->
          ({model | errorInfo = Err error}, Navigation.newUrl "#error/Delete%20failed%2e")
    UploadResponse uploadResponse ->
      case uploadResponse of
        Ok dt -> 
          let
            ddesign_ = dt.design
            tags_ = Maybe.withDefault model.tagList dt.tags
            designList = model.designList
            (index, _) = designFind ddesign_.design.designid designList.designs
            designs_ = 
              if index == noDesign then
                Array.fromList [ddesign_]
              else
                Array.set index ddesign_ designList.designs
            mainDesign_ = if index == noDesign then 0 else index
            designList_ =
              if index == noDesign then
                DesignList designs_ "" "" "" 0 1 ""
              else
                {designList | designs = designs_}
            cmd_ =
              if index == noDesign  || index /= model.mainDesign then
                Navigation.modifyUrl ("#design/" ++ (toString ddesign_.design.designid))
              else
                Navigation.back 1
          in
            ({model 
              | designList = designList_, editDesign = Nothing, errorInfo = Ok "Upload success"
              , viewMode = Designs, mainDesign = mainDesign_, tagList = tags_}, cmd_)
        Err error ->
          let
            msg = case error of
              Http.BadStatus resp -> 
                let
                  i = String.indices "</h1>" resp.body
                in case List.head i of
                  Nothing -> resp.body
                  Just found -> "<h3>Upload issue:</h3>" ++ (String.dropLeft (found + 5) resp.body)
              Http.BadPayload msg resp -> "Unexpected response."
              _ -> "Communication error."
          in
            ({model | errorInfo = Err error, errorMessage = msg}, Cmd.none)
    FileRead fpd ->
      case model.editDesign of
        Nothing -> (model, Cmd.none)
        Just edesign ->
          let
            edesign_ = Design.setFile fpd edesign
          in
            ({model | editDesign = Just edesign_}, Cmd.none)


makeQuery : Model -> String
makeQuery model =
  case (model.limitCC, model.designMode == Design.Medium) of
    (True , True ) -> "?cc&large"
    (True , False) -> "?cc"
    (False, True ) -> "?large"
    (False, False) -> "?"



-- VIEW



designString: Int -> String
designString des =
  if des < 1 then
    ""
  else
    toString des

makeIndexLink : Char -> List (Html Msg)
makeIndexLink c =
  [ a [class "letterref", href ("#titleindex/" ++ (String.fromChar c))] 
      [b [] [text (String.fromChar c)]]
  , text " "
  ]

makePNlink : String -> Int -> String -> Html Msg
makePNlink type_ count url =
  a [ href ("#" ++ url), 
      style [("visibility", if (String.isEmpty url) then "hidden" else "visible")]]
    [ b [] [text (type_ ++ " " ++ (toString count))]]

makePNbar : DesignList -> Html Msg
makePNbar dlist =
  div [class "clearleft"] 
  [ makePNlink "Previous" dlist.count dlist.prevlink
  , text " "
  , makePNlink "Next" dlist.count dlist.nextlink
  ]

makeUpBar : Bool -> DesignList -> Html Msg
makeUpBar pending dlist =
  if pending && Array.length dlist.designs > 0 then
    div [class "khomut"] [img [src "graphics/loading.gif", alt "No designs", width 216, height 216] []]
  else
    if String.isEmpty dlist.prevlink then
      text ""
    else
      div [class "khomut"]
      [ a [href "#", onNav <| LoadDesigns dlist.prevlink] 
          [img [src "graphics/more_up.png", alt "More designs", width 64, height 64] []]]

makeDownBar : Bool -> DesignList -> Html Msg
makeDownBar pending dlist =
  if pending then
    div [class "khomut"] [img [src "graphics/loading.gif", alt "No designs", width 216, height 216] []]
  else
    if String.isEmpty dlist.nextlink then
      div [class "khomut"] [img [src "graphics/khomut.png", alt "No designs", width 100, height 33] []]
    else
      div [class "khomut"]
      [ a [href "#", onNav <| LoadDesigns dlist.nextlink] 
          [img [src "graphics/more_down.png", alt "More designs", width 64, height 64] []]]


makeHeader : String -> Html Msg
makeHeader thislink =
  let
    urlparts = String.split "/" thislink
    urlname = Maybe.withDefault "" (List.head <| (List.drop 1) <| urlparts)
    name = Maybe.withDefault "What's his name" (Http.decodeUri urlname)
    namepos = if String.endsWith "s" name then
      name ++ "'"
    else
      name ++ "'s"
    -- Map spaces to non-breaking spaces
    toNB str = String.map (\c -> if c == ' ' then ' ' else c) str
    designs = toNB (namepos ++ " Designs")
    likes = toNB (namepos ++ " Likes")
  in
    if String.startsWith "title/" thislink then
      div []
      ([ a [class "letterref", href "#title/0"] [b [] [text "all"]], text " "]
      ++
      List.concat (List.map makeIndexLink (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      ++ [hr [][]])
    else if String.startsWith "faves/" thislink then
      div [class "tabtable"]
      [ div [class "tabdata", style [("background-image", "url(graphics/empty.png)")]]
           [text " "]
      , div [class "tabinter", style [("background-image", "url(graphics/empty2inactive.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/inactive.png)")]] 
           [a [href (makeUri "#user" [urlname, "0"])] [text designs]]
      , div [class "tabinter", style [("background-image", "url(graphics/inactive2active.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/active.png)")]] 
           [text likes]
      , div [class "tabinter", style [("background-image", "url(graphics/active2empty.png)")]]
           [text " "]
      , div [class "tabrest rightcell", style [("background-image", "url(graphics/empty.png)")]]
            [text ("To link to this author: [link user:" ++ name ++ "] ... [/link] ")]
      , div [class "tabclear"] []
      ]
    else if String.startsWith "user/" thislink then
      div [class "tabtable"]
      [ div [class "tabdata", style [("background-image", "url(graphics/empty.png)")]]
           [text " "]
      , div [class "tabinter", style [("background-image", "url(graphics/empty2active.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/active.png)")]] 
           [text designs]
      , div [class "tabinter", style [("background-image", "url(graphics/active2inactive.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/inactive.png)")]] 
           [a [href (makeUri "#faves" [urlname, "0"])] [text likes]]
      , div [class "tabinter", style [("background-image", "url(graphics/inactive2empty.png)")]]
           [text " "]
      , div [class "tabrest rightcell", style [("background-image", "url(graphics/empty.png)")]]
            [text ("To link to this author: [link user:" ++ name ++ "] ... [/link] ")]
      , div [class "tabclear"] []
      ]
    else 
      text ""

viewDesigns : Model -> List (Html Msg)
viewDesigns model =
  (makeHeader model.designList.thislink)
  ::
  [ (makeUpBar model.pendingLoad model.designList)
  , div [] 
    (let
      vcfg = Design.ViewConfig model.designMode model.user
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

radio : String -> msg -> Bool -> Bool -> Html msg
radio value msg isChecked isDisabled =
  label []
    [ input [ type_ "radio", name "design-size", onClick msg, checked isChecked, disabled isDisabled ] []
    , text value
    ]


viewTagInfo : TagInfo -> Html Msg
viewTagInfo tag =
  tr [] 
  [ td [align "right"] [text (toString tag.count)]
  , td [align "left"] [a [href (makeUri "#tag" [tag.name, "0"])] [text tag.name]]
  ]

viewMiniUser : MiniUser -> Html Msg
viewMiniUser muser = 
  tr []
  [ td [align "left"] [a [href (makeUri "#user" [muser.name, "0"])] [text muser.name]]
  , td [align "right"] [text (toString muser.numPosts)]
  , td [align "right"] [text (makeDate muser.joinedOn)]
  ]

view : Model -> Html Msg
view model =
  div []
  [ div [ id "CFAcolumn" ]
    [ h5 [] [ text "Gallery Tools:" ]
    , ul []
      [ li [] [ a [href "#newest/0"] [text "Newest" ]]
      , li [] [ a [href "#oldest/0"] [text "Oldest" ]]
      , li [] [ a [href "#title/0"]  [text "Title" ]]
      , li [] [ a [href "#popular/0"] [text "Most Likes" ]]
      , li [] [ a [href "#random"] [text "Random" ]]
      , li [] [ a [href "#users/name/0/25"] [text "People" ]]
      , li [] [ a [href "#tags/tag"] [text "Tags"] ]
      ]
    , h5 [] [ text "Display Mode:" ]
    , ( let
          disable = not (model.viewMode == Designs && model.mainDesign == noDesign)
        in
        ul [style [("color", if disable then "#d8cb9f" else "inherit")]]
        [ li [] 
            [ label []
              [ input
                [ type_ "checkbox", onCheck CCcheck, checked model.limitCC, disabled disable] []
              , img 
                [ class "top"
                , style [ ("padding", "0px 5px") ]
                , src "graphics/CC.badge.png"
                , alt "Creative Commons badge"
                ] []
              , text "Only"
              ]
            ]
          , li [] [text " "]
          , li [] [ text "Display size:" ]
          , fieldset []
            [ li [] [radio " Small" (SwitchTo Design.Small)  (model.designMode == Design.Small) disable]
            , li [] [radio " Large" (SwitchTo Design.Medium) (model.designMode == Design.Medium) disable]
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
                      , style [("padding", "1px")]] []
              , input [ type_ "submit", value "Go", style [("padding", "1px")]] [ ]
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
                      , style [("padding", "1px")]] []
              , input [ type_ "submit", value "Go", style [("padding", "1px")]] [ ]
              ]
            ]
          ]
        ]
      ]
    , h5 [] [ text "RSS Feeds"]
    , ul []
      [ li [ class "rss" ] [ a [ href "uploads/design_feed.xml" ] 
                               [ text "Designs" ]]
      , li [ class "rss" ] [ a [ href "uploads/comment_feed.xml" ] 
                               [ text "Comments" ]]
      ]
    , case model.user of
        Just user ->
          div []
          [ h5 [] [ text ("User " ++ user.name) ]
          , ul []
            [ li [] [ a [href "#edit/0"] [text "Upload a Design" ]]
            , li [] [ a [href (makeUri "#user" [user.name, "0"])] [text "My uploads" ]]
            , li [] [ a [ onClick LogoutClick, href "#" ] [ text "Logout" ]]
            ]
          , div [hidden True]
            [ Html.map LoginMsg (Login.view model.loginform) ]
          ]
        Nothing ->
          Html.map LoginMsg (Login.view model.loginform)
    ]
  , div [ id "CFAcontent" ]
    ( case model.viewMode of
        Error ->
          [ div [ property "innerHTML" (JE.string model.errorMessage) ] []
          ]
        Editing ->
        ( case model.editDesign of
            Nothing -> [makeDownBar True zeroList]
            Just edesign ->
              [ Html.map EDesignMsg (Design.viewEdit model.tagList edesign)
              , if model.errorMessage == "" then
                  text ""
                else
                  div [ property "innerHTML" (JE.string model.errorMessage)
                      , class "alertbox"] []
              ]
        )
        Designs ->
        ( case Array.get model.mainDesign model.designList.designs of
            Nothing ->
              viewDesigns model
            Just design ->
              let
                vc = Design.ViewConfig Design.Large model.user
              in
                [ if Array.isEmpty model.designList.designs then
                    text ""   -- TODO: fix logic
                  else
                    div [style [("float", "right"), ("position", "relative"), ("top", "-1.75em")]]
                     [a [href "#", onNav DismissDesign] [text "⬅︎ Back"]]
                , Html.map DesignMsg (Design.view vc design)
                ]
        )
        Tags ->
        ( [ table [class "tagstable"]
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
        )
        People ->
          viewUsers model
        Translation ->
        ( [text "translation"]
        )
        Default ->
        ( [ table []
            [ tr []
              [ td [class "halfcell"]
                [ text """
     CFDG is a simple language for generating stunning images.
     With only a few lines you can describe abstract art, beautiful organic scenery,
     and many kinds of fractals. It's highly addictive!
                  """
                , br [] []
                , br [] []
                , text "The CFDG Gallery is a public repository for artwork made "
                , text "using the language."
                , br [] []
                , br [] []
                , b [] [text "What now?"]
                , br [] []
                , br [] []
                , text "The links at the top of the page will navigate you around the site. "
                , a [href "../downloads.html"] [text "Get the software here. "]
                , text " or "
                , a [href "#newest/0"] [text "browse the latest designs uploaded. "]
                , text "Once you've started making your own, "
                , a [href "../phpbb/ucp.php?mode=register"] [text "join us "]
                , text "and post your own designs to the gallery."
                ]
              , td []
                [ case model.editDesign of
                    Just edesign ->
                      let
                        ddesign = Design.makeDDesign edesign.design
                      in
                        table [class "welcometable"]
                          [ tr []
                            [ td [class "thumbcell"] 
                              [ a [href ("#design/" ++ (toString ddesign.design.designid))] 
                                [ img [ class "image", src ddesign.design.thumblocation, alt "design thumbnail"] []]
                              ]
                            ]
                          , tr []
                            [ td [] 
                              [ text "The Gallery's newest contributor is: "
                              , a [href (makeUri "#user" [ddesign.design.owner, "0"])] [text ddesign.design.owner]
                              , text ", whose latest piece is called "
                              , b [] [text ddesign.design.title]
                              ]
                            ]
                          ]
                    Nothing ->
                      text ""
                ]
              ]
            ]
          , hr [] []
          , b [] [text "Most recent uploaded designs:"]
          , br [] []
          , br [] []
          ]
          ++
          ( let
              vcfg = Design.ViewConfig Design.Small model.user
            in 
              Array.toList (Array.map ((Design.view vcfg) >> (Html.map DesignMsg))
                                model.designList.designs)
          )
        )

    )
  ]




-- HTTP

post : String -> Http.Body -> JD.Decoder a -> Http.Request a
post url body decoder =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = True
    }

put : String -> Http.Body -> JD.Decoder a -> Http.Request a
put url body decoder =
  Http.request
    { method = "PUT"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = True
    }

get : String -> JD.Decoder a -> Http.Request a
get url decoder =
  Http.request
    { method = "GET"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = True
    }


resolveAction : Maybe Action -> Model -> Cmd Msg
resolveAction ma model =
  case ma of 
    Nothing -> Cmd.none
    Just act -> case act of
      UpdateComment commentid comment_ -> sendComment commentid comment_ model
      CreateComment designid comment_ -> newComment designid comment_ model
      DeleteComment commentid -> deleteComment commentid model
      AddFaves designid -> changeFave "addfave" designid model
      RemoveFaves designid -> changeFave "deletefave" designid model
      CancelEditAct -> Navigation.back 1
      DeleteDesign designid -> deleteDesign designid model
      UploadDesign -> case model.editDesign of
        Nothing -> Cmd.none
        Just edesign -> uploadDesign edesign model
      GetFile id -> fileSelected id
      _ -> Cmd.none

changeFave : String -> Int -> Model -> Cmd Msg
changeFave change designid model =
  let
    url = String.join "/" [model.backend, change, toString designid]
  in
    Http.send NewFaves (post url Http.emptyBody decodeFaves)

decodeFaves : JD.Decoder FaveInfo
decodeFaves =
  JD.map2 FaveInfo
    (JD.field "designid"  JD.int)
    (JD.field "faves" (JD.list JD.string))

sendComment : Int -> String -> Model -> Cmd Msg
sendComment commentid comment_ model =
  let
    url = model.backend ++ "/updatecomment/" ++ (toString commentid)
  in
    Http.send NewComment (put url (Http.stringBody "text/plain" comment_) Comment.decodeComment)

newComment : Int -> String -> Model -> Cmd Msg
newComment designid comment_ model =
  let
    url = model.backend ++ "/createcomment/" ++ (toString designid)
  in
    Http.send NewComment (put url (Http.stringBody "text/plain" comment_) Comment.decodeComment)

deleteComment : Int -> Model -> Cmd Msg
deleteComment commentid model =
  let
    url = model.backend ++ "/deletecomment/" ++ (toString commentid)
  in
    Http.send RemoveComment (post url Http.emptyBody decodeCommentId)

decodeCommentId : JD.Decoder Int
decodeCommentId =
  JD.field "commentid" JD.int
      
getNewbie : Model -> Cmd Msg
getNewbie model =
  let
    url = model.backend ++ "/newbie"
  in
    Http.send NewEditDesign (get url decodeEDesign)
      

getDesign : Int -> Model -> Cmd Msg
getDesign id model =
  let
    url = model.backend ++ "/design/" ++ (toString id)
  in
    Http.send NewDesign (get url decodeDesignTag)

decodeDesignTag : JD.Decoder DesignTags
decodeDesignTag =
  JD.map2 DesignTags
    (JD.field "design" Design.decodeDDesign)
    (JD.maybe <| JD.field "tags" (JD.list decodeTagInfo))

loadEditDesign : Int -> Model -> Cmd Msg
loadEditDesign id model =
  let
    url = model.backend ++ "/design/" ++ (toString id)
  in
    Http.send NewEditDesign (get url decodeEDesign)

decodeEDesign : JD.Decoder Design.EditDesign
decodeEDesign =
   JD.field "design" Design.decodeEDesign

uploadDesign : Design.EditDesign -> Model -> Cmd Msg
uploadDesign edesign model =
  let
    url = model.backend ++ "/postdesign"
    jbody = Design.encodeDesign edesign      
  in
    Http.send UploadResponse (post url (Http.jsonBody jbody) decodeDesignTag)

getDesigns : Model -> String -> Int -> Int -> Cmd Msg
getDesigns model query start count =
  let
    ccquery = if model.limitCC then "cc" ++ query else query
    url = String.join "/" [model.backend, ccquery, 
                           toString(start), toString(count)]
  in
    Http.send NewDesigns (get url decodeDesigns)

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

deleteDesign : Int -> Model -> Cmd Msg
deleteDesign designid model =
  let
    url = model.backend ++ "/delete/" ++ (toString designid)
  in
    Http.send DeleteADesign (post url Http.emptyBody decodeDesignId)

decodeDesignId : JD.Decoder Int
decodeDesignId = 
  JD.field "designid" JD.int


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
    Http.send NewUser (post url Http.emptyBody decodeUser)

loginSession : Model -> Cmd Msg
loginSession model =
  let
    url = model.backend ++ "/userinfo"
  in
    Http.send SessionUser (get url decodeUser)

decodeUser : JD.Decoder User.User
decodeUser =
  JD.field "userinfo" User.decodeUser

logoutUser : Model -> Cmd Msg
logoutUser model = 
  let
    url = model.backend ++ "/logout"
  in
    Http.send LogoutUser (post url Http.emptyBody decodeUserLogout)

decodeUserLogout : JD.Decoder Bool
decodeUserLogout =
  JD.field "logout_success" JD.bool

getCfdg : Int -> Model -> Cmd Msg
getCfdg id model =
  let
    url = model.backend ++ "/data/cfdg/" ++ (toString id)
  in
    Http.send (ReceiveCfdg id) (Http.getString url)

getCfdgfromDesign : Model -> Design.DisplayDesign -> Cmd Msg
getCfdgfromDesign model ddesign =
  getCfdg ddesign.design.designid model

getInfo : Int -> Model -> Cmd Msg
getInfo id model =
  let
    url = model.backend ++ "/auxinfo/" ++ (toString id)
  in
    Http.send (ReceiveInfo id) (get url decodeInfo)

decodeInfo : JD.Decoder DesignInfo
decodeInfo =
  JD.map3 DesignInfo
    (JD.field "fans" (JD.list JD.string))
    (JD.field "tags" (JD.list JD.string))
    (JD.maybe <| JD.field "imagesize" Design.decodeSize)

getComments : Int -> Model -> Cmd Msg
getComments id model =
  let
    url = model.backend ++ "/comments/" ++ (toString id)
  in
    Http.send NewComments (get url decodeComments)

decodeComments : JD.Decoder (List Comment.Comment)
decodeComments =
  JD.field "comments" (JD.list Comment.decodeComment)

getTitle : Model -> String -> Cmd Msg
getTitle model title =
  let
    url = 
      if model.limitCC then
        model.backend ++ "/cctitleindex/" ++ (Http.encodeUri title)
      else
        model.backend ++ "/titleindex/" ++ (Http.encodeUri title)
  in
    Http.send GotTitleIndex (get url decodeTitleIndex)

decodeTitleIndex : JD.Decoder Int
decodeTitleIndex = 
  JD.field "index" JD.int

getTags : Model -> Cmd Msg
getTags model = 
  let
    url = model.backend ++ "/tags"
  in
    Http.send GotTags (get url decodeTags)

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
                           toString(start), toString(count)]
  in
    Http.send NewUsers (get url decodeUsers)

decodeUsers : JD.Decoder UserList
decodeUsers =
  JD.map5 UserList
    (JD.field "users"    (JD.list User.decodeMiniUser))
    (JD.field "prevlink"  JD.string)
    (JD.field "nextlink"  JD.string)
    (JD.field "thislink"  JD.string)
    (JD.field "count"     JD.int)
      

-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  fileContentRead FileRead
