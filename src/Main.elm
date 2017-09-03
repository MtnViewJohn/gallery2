import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onSubmit, onInput, onWithOptions)
import String exposing (trimLeft, toInt)
import Login
import Design
import Comment
import User exposing (..)
import Json.Decode
import Http
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Random
import GalleryUtils exposing (..)
import Time exposing (Time)
import Task
import Array exposing (Array)
import Ports exposing (FilePortData, fileSelected, fileContentRead)

main : Program Never Model Msg
main =
  Navigation.program
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
  , count : Int
  }

type alias UserList =
  { users : List User.MiniUser
  , prevlink : String
  , nextlink : String
  , thislink : String
  , count : Int
  }

type alias TagInfo =
  { name : String
  , count : Int
  }

type alias FaveInfo =
  { designid : Int
  , fans : List String
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
  , editDesign : Maybe Design.EditDesign
  , designMode : Design.ViewSize
  , tagList : List TagInfo
  , userList : UserList
  , errorMessage : String
  , initUrl : String
  }

zeroList : DesignList
zeroList = DesignList Array.empty "" "" "" 0

zeroUList : UserList
zeroUList = UserList [] "" "" "" 0

initModel : Navigation.Location -> (Model, Cmd Msg)
initModel loc = 
  (Model Nothing Login.initModel False "" 0 Designs noDesign zeroList Nothing Design.Small [] zeroUList "" loc.href, 
    loginSession)

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
      
      

-- URL PARSING


type Route
  = Home
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
  = LoginClick 
  | LogoutClick
  | CCcheck Bool
  | SwitchTo Design.ViewSize
  | NewURL Navigation.Location
  | LoginMsg Login.Msg
  | DesignMsg Design.MsgId
  | EDesignMsg Design.EMsg
  | LookupName
  | LookupDesign
  | AuthorText String
  | DesignText String
  | NewSeed Int
  | DismissDesign
  | LoadDesign Time
  | NewDesign (Result Http.Error Design.DisplayDesign)
  | NewEditDesign (Result Http.Error Design.EditDesign)
  | NewDesigns (Result Http.Error DesignList)
  | NewUser (Result Http.Error User.User)
  | SessionUser (Result Http.Error User.User)
  | LogoutUser (Result Http.Error Bool)
  | ReceiveCfdg Int (Result Http.Error String)
  | NewComments (Result Http.Error (List Comment.Comment))
  | NewComment (Result Http.Error Comment.Comment)
  | RemoveComment (Result Http.Error Int)
  | GotTitleIndex (Result Http.Error Int)
  | GotTags (Result Http.Error (List TagInfo))
  | NewUsers (Result Http.Error UserList)
  | NewFaves (Result Http.Error FaveInfo)
  | DeleteADesign (Result Http.Error Int)
  | UploadResponse (Result Http.Error Design.DisplayDesign)
  | FileRead FilePortData

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoginClick ->
      (model, loginUser model.loginform)
    LogoutClick ->
      (model, logoutUser)
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
    NewURL loc ->
      let
        qbits = String.split "&" (String.dropLeft 1 loc.search)
        cc_ = List.member "cc" qbits
        mode_ = if (List.member "large" qbits) then Design.Medium else Design.Small
        model_ = {model | limitCC = cc_, designMode = mode_}
        dcount = if model_.designMode == Design.Small then 50 else 5
      in
        case Url.parseHash route loc of
          Nothing ->
            (model_, Cmd.none)
          Just route ->
            case route of
              Home ->
                ({model_ | viewMode = Default
                         , mainDesign = noDesign
                         , designList = zeroList}, 
                  Cmd.batch [getDesigns model_ "newest" 0 10, getNewbie])
              ErrorMsg msg_enc ->
                let
                  msg = Maybe.withDefault "Malformed error message." (Http.decodeUri msg_enc)
                in
                  ({model_ | errorMessage = msg, viewMode = Error}, Cmd.none)
              DesignID id ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesign id)
              EditDesign id ->
                case model_.user of
                  Nothing -> (model_, Cmd.none)
                  Just user ->
                    if id == 0 then
                      ({model_ | editDesign = Nothing, viewMode = Editing}
                      , Task.perform LoadDesign Time.now)
                    else
                      let
                        (index, mddesign) = designFind id model_.designList.designs
                      in case mddesign of
                          Nothing ->
                            ({model_ | editDesign = Nothing, viewMode = Editing},
                              loadEditDesign id)
                          Just ddesign ->
                            let
                              edesign_ = Design.makeEDesign ddesign.design
                            in
                              ({model_ | editDesign = Just edesign_, viewMode = Editing}
                              , Cmd.none)
              Author name_enc start count ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  ({model_ | mainDesign = noDesign, viewMode = Designs },
                    getDesigns model_ (makeUri "by" [name]) start count)
              AuthorInit name_enc start ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  ({model_ | mainDesign = noDesign, viewMode = Designs },
                    getDesigns model_ (makeUri "by" [name]) start dcount)
              AuthorInit2 name_enc ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  (model_, Navigation.modifyUrl (makeUri "#user" [name, "0"]))
              Faves name_enc start count ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  ({model_ | mainDesign = noDesign, viewMode = Designs },
                    getDesigns model_ (makeUri "faves" [name]) start count)
              FavesInit name_enc start ->
                let
                  name = Maybe.withDefault "" (Http.decodeUri name_enc)
                in
                  ({model_ | mainDesign = noDesign, viewMode = Designs },
                    getDesigns model_ (makeUri "faves" [name]) start dcount)
              Newest start count ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "newest" start count)
              NewestInit start ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "newest" start dcount)
              Oldest start count ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "oldest" start count)
              OldestInit start ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "oldest" start dcount)
              Title start count ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "title" start count)
              TitleInit start ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "title" start dcount)
              TitleIndex title ->
                (model_, getTitle model_ title)
              Popular start count ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "popular" start count)
              PopularInit start ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ "popular" start dcount)
              RandomDes seed start count ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ ("random/" ++ (toString seed)) start count)
              RandomInit seed start ->
                ({model_ | mainDesign = noDesign, viewMode = Designs },
                  getDesigns model_ ("random/" ++ (toString seed)) start dcount)
              RandomSeed ->
                (model_, Random.generate NewSeed (Random.int 1 1000000000))
              Tag tag_enc start count ->
                let
                  tag = Maybe.withDefault "" (Http.decodeUri tag_enc)
                in
                  ({model_ | mainDesign = noDesign, viewMode = Designs },
                    getDesigns model_ (makeUri "tag" [tag]) start count)
              TagInit tag_enc start ->
                let
                  tag = Maybe.withDefault "" (Http.decodeUri tag_enc)
                in
                  ({model_ | mainDesign = noDesign, viewMode = Designs },
                    getDesigns model_ (makeUri "tag" [tag]) start dcount)
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
                if List.member utype ["name", "posts", "joined"] then
                  ({model_ | viewMode = People}, getUsers ("users/" ++ utype) start count)
                else
                  (model_, Cmd.none)
    NewSeed seed ->
      (model, Navigation.newUrl ("#random/" ++ (toString seed ++ "/0")))
    LoginMsg lMsg ->
      let
        (newLoginModel, maybeAction) = Login.update lMsg model.loginform          
      in
        case maybeAction of
          Just action ->
            update LoginClick { model | loginform = newLoginModel }
          Nothing ->
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
            in
              (model_, resolveAction act_ model_)
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
        Ok design ->
          let
            designList_ = DesignList (Array.fromList [design]) "" "" "" 1
          in
            ({model | designList = designList_, mainDesign = 0},
              Cmd.batch [getComments design.design.designid, getCfdg design.design.designid])
        Err error ->
          ({ model | mainDesign = noDesign}, Cmd.none)
    NewEditDesign designResult ->
      case designResult of
        Ok design ->
          ({model | editDesign = Just design}, Cmd.none)
        Err error ->
          ({ model | editDesign = Nothing}, Cmd.none)
    NewDesigns designResult ->
      case designResult of
        Ok designs ->
          ({model | designList = designs, mainDesign = noDesign},         
            if model.designMode == Design.Small then
              Cmd.none
            else
              Cmd.batch (List.map getCfdgfromDesign (Array.toList designs.designs)))
        Err error ->
          ({model | designList = zeroList}, Cmd.none)
    NewUser loginResult ->
      case loginResult of
        Ok user ->
          ({model | user = Just user}, Cmd.none)
        Err error ->
          let
            newLoginModel = Login.fail "Login failed" model.loginform          
          in
            ({ model | loginform = newLoginModel }, Cmd.none) 
    SessionUser loginResult ->
      case loginResult of
        Ok user ->
          ({model | user = Just user}, getTags)
        Err error ->
          (model, getTags)
    LogoutUser logoutResult ->
      case logoutResult of
        Ok yes ->
          ({model | user = Nothing}, Cmd.none)
        Err error ->
          let
            newLoginModel = Login.fail "Logout failed" model.loginform          
          in
            ({ model | loginform = newLoginModel}, Cmd.none) 
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
                ({model | designList = designList_}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
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
                ({model | designList = designList_}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
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
              ({model | designList = designList_}, Cmd.none)
        Err _ -> (model, Cmd.none)
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
              ({model | designList = designList_}, Cmd.none)
        Err _ -> (model, Cmd.none)
    GotTitleIndex indexResult ->
      case indexResult of
        Ok index ->
          (model, Navigation.newUrl ("#title/" ++ (toString index)))
        Err _ ->
          (model, Cmd.none)
    GotTags tagsResult ->
      case tagsResult of
        Ok tags ->
          ({ model | tagList = tags}, Navigation.modifyUrl model.initUrl)
        Err error ->
          {- let
            _ = case error of
              Http.BadUrl url -> Debug.log "BadUrl" url
              Http.Timeout -> Debug.log "Timeout" ""
              Http.NetworkError -> Debug.log "NetworkError" ""
              Http.BadStatus resp -> Debug.log "BadStatus" resp.status.message
              Http.BadPayload msg resp -> Debug.log "BadPayload" msg
          in -}
            (model, Navigation.modifyUrl model.initUrl)
    NewUsers usersResult ->
      case usersResult of
        Ok users ->
          ({model | userList = users}, Cmd.none)
        Err error ->
          (model,Cmd.none)
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
              ({model | designList = designList_}, Cmd.none)
          else
            (model, Cmd.none)         -- Shouldn't happen
        Nothing -> (model, Cmd.none)  -- ditto
      Err _ -> (model, Cmd.none)
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
            ({model | designList = designList_, mainDesign = noDesign}, cmd_)
        Err error ->
          (model, Navigation.newUrl "#error/Delete%20failed%2e")
    UploadResponse uploadResponse ->
      case uploadResponse of
        Ok ddesign_ -> 
          let
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
                DesignList designs_ "" "" "" 1
              else
                {designList | designs = designs_}
            cmd_ =
              if index == noDesign  || index /= model.mainDesign then
                Navigation.modifyUrl ("#design/" ++ (toString ddesign_.design.designid))
              else
                Navigation.back 1
          in
            ({model 
              | designList = designList_, editDesign = Nothing
              , viewMode = Designs, mainDesign = mainDesign_}, cmd_)
        Err error ->
          let
            _ = case error of
              Http.BadUrl url -> Debug.log "BadUrl" url
              Http.Timeout -> Debug.log "Timeout" ""
              Http.NetworkError -> Debug.log "NetworkError" ""
              Http.BadStatus resp -> Debug.log "BadStatus" resp.status.message
              Http.BadPayload msg resp -> Debug.log "BadPayload" msg
          in
            (model, Navigation.newUrl "#error/Upload%20failed%2e")
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
      [text (String.fromChar c)]
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


makeHeader : String -> Html Msg
makeHeader thislink =
  let
    urlparts = String.split "/" thislink
    urlname = Maybe.withDefault "" (List.head <| (List.drop 1) <| urlparts)
    name = Maybe.withDefault "" (Http.decodeUri urlname)
    -- Map spaces to non-breaking spaces
    namenb = String.map (\c -> if c == ' ' then ' ' else c) name

  in
    if String.startsWith "title/" thislink then
      div []
      ([ a [class "letterref", href "#title/0"] [text "all"], text " "]
      ++
      List.concat (List.map makeIndexLink (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      ++ [hr [][]])
    else if String.startsWith "faves/" thislink then
      div [class "tabtable"]
      [ div [class "tabdata", style [("background-image", "url(graphics/empty.png)")]]
           [text ("Gallery user: " ++ namenb)]
      , div [class "tabinter", style [("background-image", "url(graphics/empty2inactive.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/inactive.png)")]] 
           [a [href (makeUri "#user" [urlname, "0"])] [text "Own Designs"]]
      , div [class "tabinter", style [("background-image", "url(graphics/inactive2active.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/active.png)")]] 
           [text "Favorite Designs"]
      , div [class "tabinter", style [("background-image", "url(graphics/active2empty.png)")]]
           [text " "]
      , div [class "tabrest", style [("background-image", "url(graphics/empty.png)")]] [text " "]
      , div [class "tabclear"] []
      ]
    else if String.startsWith "user/" thislink then
      div [class "tabtable"]
      [ div [class "tabdata", style [("background-image", "url(graphics/empty.png)")]]
           [text ("Gallery user: " ++ namenb)]
      , div [class "tabinter", style [("background-image", "url(graphics/empty2active.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/active.png)")]] 
           [text "Own Designs"]
      , div [class "tabinter", style [("background-image", "url(graphics/active2inactive.png)")]]
           [text " "]
      , div [class "tabdata", style [("background-image", "url(graphics/inactive.png)")]] 
           [a [href (makeUri "#faves" [urlname, "0"])] [text "Favorite Designs"]]
      , div [class "tabinter", style [("background-image", "url(graphics/inactive2empty.png)")]]
           [text " "]
      , div [class "tabrest", style [("background-image", "url(graphics/empty.png)")]] [text " "]
      , div [class "tabclear"] []
      ]
    else 
      text ""

viewDesigns : Model -> List (Html Msg)
viewDesigns model =
  (makeHeader model.designList.thislink)
  ::
  if Array.isEmpty model.designList.designs then
    [ div [class "khomut"] [img [src "graphics/khomut.png", alt "No designs", width 100] []]]
  else
    (makePNbar model.designList)
    ::
    ( let
        vcfg = Design.ViewConfig model.designMode model.user

        htmlList = Array.toList
          (Array.map ((Design.view vcfg) >> (Html.map DesignMsg))
            model.designList.designs)
      in
        if model.designMode == Design.Medium then
          (List.intersperse (hr [] []) htmlList)
        else
          htmlList
    )
    ++
    [makePNbar model.designList]


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
          [ th [align "left", class "usernames"] [a [href "#users/name/0/25"] [text "User name"]]
          , th [align "right"] [a [href "#users/posts/0/25"] [text "Post count"]]
          , th [align "right", class "dates"] [a [href "#users/joined/0/25"] [text "Join date"]]
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
      , li [] [ a [href "#popular/0"] [text "Popular" ]]
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
          , li [] [ text "Display size:" ]
          , li []
            [ fieldset []
              [ radio " Small" (SwitchTo Design.Small)  (model.designMode == Design.Small) disable
              , text " "
              , radio " Large" (SwitchTo Design.Medium) (model.designMode == Design.Medium) disable
              ]
            ]
          ])
    , h5 [] [ text "Lookup" ]
    , ul []
      [ li [] 
        [ Html.form [ onSubmit LookupName ]
          [ fieldset []
            [ label []
              [ text "Author "
              , input [ type_ "text", name "by", size 8, placeholder "name", 
                        value model.authorLookup, onInput AuthorText] []
              , input [ type_ "submit", value "Go" ] [ ]
              ]
            ]
          ]
        ]
      , li []
        [ Html.form [ onSubmit LookupDesign ]
          [ fieldset []
            [ label []
              [ text "Design "
              , input [ type_ "text", name "id", size 8, placeholder "id #",
                        value (designString model.designLookup), onInput DesignText ] []
              , input [ type_ "submit", value "Go"] [ ]
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
          ]
        Nothing ->
          Html.map LoginMsg (Login.view model.loginform)
    ]
  , div [ id "CFAcontent" ]
    ( case model.viewMode of
        Error ->
          [ h1 [] [text "An error occured!"]
          , p [] [text model.errorMessage]
          ]
        Editing ->
        ( case model.editDesign of
            Nothing -> [text "Design didn't load."]
            Just edesign ->
              [Html.map EDesignMsg (Design.viewEdit edesign)]
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
                [ case Array.get model.mainDesign model.designList.designs of
                    Just ddesign ->
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

post : String -> Http.Body -> Json.Decode.Decoder a -> Http.Request a
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

put : String -> Http.Body -> Json.Decode.Decoder a -> Http.Request a
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

get : String -> Json.Decode.Decoder a -> Http.Request a
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
      UpdateComment commentid comment_ -> sendComment commentid comment_
      CreateComment designid comment_ -> newComment designid comment_
      DeleteComment commentid -> deleteComment commentid
      AddFaves designid -> changeFave "addfave" designid
      RemoveFaves designid -> changeFave "deletefave" designid
      CancelEditAct -> Navigation.back 1
      DeleteDesign designid -> deleteDesign designid
      UploadDesign -> case model.editDesign of
        Nothing -> Cmd.none
        Just edesign -> uploadDesign edesign
      GetFile id -> fileSelected id
      _ -> Cmd.none

changeFave : String -> Int -> Cmd Msg
changeFave change designid =
  let
    url = String.join "/" ["http://localhost:5000", change, toString designid]
  in
    Http.send NewFaves (post url Http.emptyBody decodeFaves)

decodeFaves : Json.Decode.Decoder FaveInfo
decodeFaves =
  Json.Decode.map2 FaveInfo
    (Json.Decode.field "designid"  Json.Decode.int)
    (Json.Decode.field "faves" (Json.Decode.list Json.Decode.string))

sendComment : Int -> String -> Cmd Msg
sendComment commentid comment_ =
  let
    url = "http://localhost:5000/updatecomment/" ++ (toString commentid)
  in
    Http.send NewComment (put url (Http.stringBody "text/plain" comment_) Comment.decodeComment)

newComment : Int -> String -> Cmd Msg
newComment designid comment_ =
  let
    url = "http://localhost:5000/createcomment/" ++ (toString designid)
  in
    Http.send NewComment (put url (Http.stringBody "text/plain" comment_) Comment.decodeComment)

deleteComment : Int -> Cmd Msg
deleteComment commentid =
  let
    url = "http://localhost:5000/deletecomment/" ++ (toString commentid)
  in
    Http.send RemoveComment (post url Http.emptyBody decodeCommentId)

decodeCommentId : Json.Decode.Decoder Int
decodeCommentId =
  Json.Decode.field "commentid" Json.Decode.int
      
getNewbie : Cmd Msg
getNewbie =
  let
    url = "http://localhost:5000/newbie"
  in
    Http.send NewDesign (get url decodeDesign)
      

getDesign : Int -> Cmd Msg
getDesign id =
  let
    url = "http://localhost:5000/design/" ++ (toString id)
  in
    Http.send NewDesign (get url decodeDesign)

loadEditDesign : Int -> Cmd Msg
loadEditDesign id =
  let
    url = "http://localhost:5000/design/" ++ (toString id)
  in
    Http.send NewEditDesign (get url decodeEDesign)

decodeDesign : Json.Decode.Decoder Design.DisplayDesign
decodeDesign =
   Json.Decode.field "design" Design.decodeDDesign

decodeEDesign : Json.Decode.Decoder Design.EditDesign
decodeEDesign =
   Json.Decode.field "design" Design.decodeEDesign

uploadDesign : Design.EditDesign -> Cmd Msg
uploadDesign edesign =
  let
    url = "http://localhost:5000/postdesign"
    jbody = Design.encodeDesign edesign      
  in
    Http.send UploadResponse (post url (Http.jsonBody jbody) decodeDesign)

getDesigns : Model -> String -> Int -> Int -> Cmd Msg
getDesigns model query start count =
  let
    ccquery = if model.limitCC then "cc" ++ query else query
    url = String.join "/" ["http://localhost:5000", ccquery, 
                           toString(start), toString(count)]
  in
    Http.send NewDesigns (get url decodeDesigns)

decodeDesigns : Json.Decode.Decoder DesignList
decodeDesigns = 
  Json.Decode.map5 DesignList
    (Json.Decode.field "designs" (Json.Decode.array Design.decodeDDesign))
    (Json.Decode.field "prevlink" Json.Decode.string)
    (Json.Decode.field "nextlink" Json.Decode.string)
    (Json.Decode.field "thislink" Json.Decode.string)
    (Json.Decode.field "count"    Json.Decode.int)

deleteDesign : Int -> Cmd Msg
deleteDesign designid =
  let
    url = "http://localhost:5000/delete/" ++ (toString designid)
  in
    Http.send DeleteADesign (post url Http.emptyBody decodeDesignId)

decodeDesignId : Json.Decode.Decoder Int
decodeDesignId = 
  Json.Decode.field "designid" Json.Decode.int


loginUser : Login.Model -> Cmd Msg
loginUser lmodel =
  let
    url = makeUri 
      "http://localhost:5000/login"
      [ lmodel.user
      , lmodel.password
      , if lmodel.remember then "1" else "0"
      ]
  in
    Http.send NewUser (post url Http.emptyBody decodeUser)

loginSession : Cmd Msg
loginSession =
  let
    url = "http://localhost:5000/userinfo"
  in
    Http.send SessionUser (get url decodeUser)

decodeUser : Json.Decode.Decoder User.User
decodeUser =
  Json.Decode.field "userinfo" User.decodeUser

logoutUser : Cmd Msg
logoutUser = 
  let
    url = "http://localhost:5000/logout"
  in
    Http.send LogoutUser (post url Http.emptyBody decodeUserLogout)

decodeUserLogout : Json.Decode.Decoder Bool
decodeUserLogout =
  Json.Decode.field "logout_success" Json.Decode.bool

getCfdg : Int -> Cmd Msg
getCfdg id =
  let
    url = "http://localhost:5000/data/cfdg/" ++ (toString id)
  in
    Http.send (ReceiveCfdg id) (Http.getString url)

getCfdgfromDesign : Design.DisplayDesign -> Cmd Msg
getCfdgfromDesign ddesign =
  getCfdg ddesign.design.designid

getComments : Int -> Cmd Msg
getComments id =
  let
    url = "http://localhost:5000/comments/" ++ (toString id)
  in
    Http.send NewComments (get url decodeComments)

decodeComments : Json.Decode.Decoder (List Comment.Comment)
decodeComments =
  Json.Decode.field "comments" (Json.Decode.list Comment.decodeComment)

getTitle : Model -> String -> Cmd Msg
getTitle model title =
  let
    url = 
      if model.limitCC then
        "http://localhost:5000/cctitleindex/" ++ (Http.encodeUri title)
      else
        "http://localhost:5000/titleindex/" ++ (Http.encodeUri title)
  in
    Http.send GotTitleIndex (get url decodeTitleIndex)

decodeTitleIndex : Json.Decode.Decoder Int
decodeTitleIndex = 
  Json.Decode.field "index" Json.Decode.int

getTags : Cmd Msg
getTags = 
  let
    url = "http://localhost:5000/tags"
  in
    Http.send GotTags (get url decodeTags)

decodeTags : Json.Decode.Decoder (List TagInfo)
decodeTags =
  Json.Decode.field "tags" (Json.Decode.list decodeTagInfo)

decodeTagInfo : Json.Decode.Decoder TagInfo
decodeTagInfo =
  Json.Decode.map2 TagInfo
    (Json.Decode.field "name"  Json.Decode.string)
    (Json.Decode.field "count" Json.Decode.int)

      
getUsers: String -> Int -> Int -> Cmd Msg
getUsers query start count =
  let
    url = String.join "/" ["http://localhost:5000", query, 
                           toString(start), toString(count)]
  in
    Http.send NewUsers (get url decodeUsers)

decodeUsers : Json.Decode.Decoder UserList
decodeUsers =
  Json.Decode.map5 UserList
    (Json.Decode.field "users"    (Json.Decode.list User.decodeMiniUser))
    (Json.Decode.field "prevlink"  Json.Decode.string)
    (Json.Decode.field "nextlink"  Json.Decode.string)
    (Json.Decode.field "thislink"  Json.Decode.string)
    (Json.Decode.field "count"     Json.Decode.int)
      

-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  fileContentRead FileRead
