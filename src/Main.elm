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

main : Program Never Model Msg
main =
  Navigation.program
    NewURL
    { init = initModel
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }



-- MODEL

type ViewMode
  = Designs
  | Tags
  | People
  | Translation
  | Default

type alias DesignList =
  { designs : List Design.Design
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

type alias Model =
  { user : Maybe User
  , loginform : Login.Model
  , limitCC : Bool
  , authorLookup : String
  , designLookup : Int
  , viewMode : ViewMode
  , mainDesign : Maybe Design.Design
  , designList : DesignList
  , designMode : Design.ViewSize
  , tagList : List TagInfo
  , userList : UserList
  }

zeroList : DesignList
zeroList = DesignList [] "" "" "" 0

zeroUList : UserList
zeroUList = UserList [] "" "" "" 0

initModel : Navigation.Location -> (Model, Cmd Msg)
initModel loc = 
  (Model Nothing Login.initModel False "" 0 Designs Nothing zeroList Design.Small [] zeroUList, 
    Cmd.batch [loginSession, getTags, Navigation.modifyUrl loc.href])

designMap : (Design.Design -> Design.Design) -> DesignList -> DesignList
designMap mapping oldList =
  { oldList | designs = List.map mapping oldList.designs }


-- URL PARSING


type Route
  = Home
  | DesignID Int
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
    , Url.map DesignID (Url.s "design" </> int)
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
  | DesignMsg Design.Msg
  | LookupName
  | LookupDesign
  | AuthorText String
  | DesignText String
  | NewSeed Int
  | NewDesign (Result Http.Error Design.Design)
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoginClick ->
      (model, loginUser model.loginform)
    LogoutClick ->
      (model, logoutUser)
    CCcheck cc ->
      ({ model | limitCC = cc }, Cmd.none)
    SwitchTo size ->
      ({model | designMode = size}, Navigation.modifyUrl ("#" ++ model.designList.thislink))
    NewURL loc ->
      case Url.parseHash route loc of
        Nothing ->
          (model, Cmd.none)
        Just route ->
          case route of
            Home ->
              ({model | viewMode = Default
                      , mainDesign = Nothing
                      , designList = zeroList}, 
                Cmd.batch [getDesigns "newest" 0 10, getNewbie])
            DesignID id ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesign id)
            Author name_enc start count ->
              let
                name = Maybe.withDefault "" (Http.decodeUri name_enc)
              in
                ({model | mainDesign = Nothing, viewMode = Designs },
                  getDesigns (makeUri "by" [name]) start count)
            AuthorInit name_enc start ->
              let
                name = Maybe.withDefault "" (Http.decodeUri name_enc)
              in
                ({model | mainDesign = Nothing, viewMode = Designs },
                  getDesigns (makeUri "by" [name]) start (if model.designMode == Design.Small then 50 else 5))
            AuthorInit2 name_enc ->
              let
                name = Maybe.withDefault "" (Http.decodeUri name_enc)
              in
                (model, Navigation.modifyUrl (makeUri "#user" [name, "0"]))
            Faves name_enc start count ->
              let
                name = Maybe.withDefault "" (Http.decodeUri name_enc)
              in
                ({model | mainDesign = Nothing, viewMode = Designs },
                  getDesigns (makeUri "faves" [name]) start count)
            FavesInit name_enc start ->
              let
                name = Maybe.withDefault "" (Http.decodeUri name_enc)
              in
                ({model | mainDesign = Nothing, viewMode = Designs },
                  getDesigns (makeUri "faves" [name]) start (if model.designMode == Design.Small then 50 else 5))
            Newest start count ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "newest" start count)
            NewestInit start ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "newest" start (if model.designMode == Design.Small then 50 else 5))
            Oldest start count ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "oldest" start count)
            OldestInit start ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "oldest" start (if model.designMode == Design.Small then 50 else 5))
            Title start count ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "title" start count)
            TitleInit start ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "title" start (if model.designMode == Design.Small then 50 else 5))
            TitleIndex title ->
              (model, getTitle title)
            Popular start count ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "popular" start count)
            PopularInit start ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns "popular" start (if model.designMode == Design.Small then 50 else 5))
            RandomDes seed start count ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns ("random/" ++ (toString seed)) start count)
            RandomInit seed start ->
              ({model | mainDesign = Nothing, viewMode = Designs },
                getDesigns ("random/" ++ (toString seed)) start (if model.designMode == Design.Small then 50 else 5))
            RandomSeed ->
              (model, Random.generate NewSeed (Random.int 1 1000000000))
            Tag tag_enc start count ->
              let
                tag = Maybe.withDefault "" (Http.decodeUri tag_enc)
              in
                ({model | mainDesign = Nothing, viewMode = Designs },
                  getDesigns (makeUri "tag" [tag]) start count)
            TagInit tag_enc start ->
              let
                tag = Maybe.withDefault "" (Http.decodeUri tag_enc)
              in
                ({model | mainDesign = Nothing, viewMode = Designs },
                  getDesigns (makeUri "tag" [tag]) start (if model.designMode == Design.Small then 50 else 5))
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
                ({model| tagList = List.sortWith comp model.tagList, viewMode = Tags}, Cmd.none)
            Users utype start count ->
              if List.member utype ["name", "posts", "joined"] then
                ({model | viewMode = People}, getUsers ("users/" ++ utype) start count)
              else
                (model, Cmd.none)
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
    DesignMsg dmsg ->
      case model.mainDesign of
        Nothing -> (model, Cmd.none)
        Just des ->
          let
            (newDesign, maybeAction) = Design.update dmsg des
            (newDesigns, maybeActions) = 
              List.unzip (List.map (Design.update dmsg) model.designList.designs)
            newAction = firstJust (maybeAction :: maybeActions)
          in
            ({model|mainDesign = Just newDesign}, resolveAction newAction model)
    LookupName ->
      (model, Navigation.newUrl (makeUri "#user" [model.authorLookup, "0"]))
    LookupDesign ->
      ({model | designList = zeroList}, 
        Navigation.newUrl ("#design/" ++ (toString model.designLookup)))
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
    NewDesign designResult ->
      case designResult of
        Ok design ->
          ({ model | mainDesign = Just design}, 
            Cmd.batch [getComments design.designid, getCfdg design.designid])
        Err error ->
          ({ model | mainDesign = Nothing}, Cmd.none)
    NewDesigns designResult ->
      case designResult of
        Ok designs ->
          ({model | designList = designs},         
            if model.designMode == Design.Small then
              Cmd.none
            else
              Cmd.batch (List.map getCfdgfromDesign designs.designs))
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
          ({model | user = Just user}, Cmd.none)
        Err error ->
          (model, Cmd.none)
    LogoutUser logoutResult ->
      case logoutResult of
        Ok yes ->
          ({model | user = Nothing}, Cmd.none)
        Err error ->
          let
            newLoginModel = Login.fail "Logout failed" model.loginform          
          in
            ({ model | loginform = newLoginModel }, Cmd.none) 
    ReceiveCfdg id cfdgResult ->
      case cfdgResult of
        Ok cfdgText ->
          let
            setCfdg = Design.setCfdg id cfdgText
          in
            ( { model
              | mainDesign = Maybe.map setCfdg model.mainDesign
              , designList = designMap setCfdg model.designList
              }, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    NewComments commentResult ->
      case commentResult of
        Ok comments ->
          case model.mainDesign of
            Just oldDesign ->
              let
                newDesign = Design.setComments comments oldDesign
              in
                ({model | mainDesign = Just newDesign}, Cmd.none)
            Nothing ->            -- Shouldn't happen
              (model, Cmd.none)   -- Just drop it
        Err _ ->
          (model, Cmd.none)
    NewComment commentResult ->
      case commentResult of
        Ok comment -> case model.mainDesign of
          Just design ->
            let
              design_ = Design.setComment comment design
            in
              ({model | mainDesign = Just design_}, Cmd.none)
          Nothing -> (model, Cmd.none)
        Err _ -> (model, Cmd.none)
    RemoveComment commentidResult ->
      case commentidResult of
        Ok commentid -> case model.mainDesign of
          Just design ->
            let
              design_ = Design.removeComment commentid design
            in
              ({model | mainDesign = Just design_}, Cmd.none)
          Nothing -> (model, Cmd.none)
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
          ({ model | tagList = tags}, Cmd.none)
        Err error ->
          {- let
            _ = case error of
              Http.BadUrl url -> Debug.log "BadUrl" url
              Http.Timeout -> Debug.log "Timeout" ""
              Http.NetworkError -> Debug.log "NetworkError" ""
              Http.BadStatus resp -> Debug.log "BadStatus" resp.status.message
              Http.BadPayload msg resp -> Debug.log "BadPayload" msg
          in -}
            (model, Cmd.none)
    NewUsers usersResult ->
      case usersResult of
        Ok users ->
          ({model | userList = users}, Cmd.none)
        Err error ->
          (model,Cmd.none)





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
  if List.isEmpty model.designList.designs then
    [ text "Nothing to show" ]
  else
    (makePNbar model.designList)
    ::
    ( let
        vcfg = Design.ViewConfig model.designMode model.user

        htmlList = 
          (List.map ((Design.view vcfg) >> (Html.map DesignMsg))
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
      , li [] 
        [ label []
          [ input [ type_ "checkbox", onCheck CCcheck, checked model.limitCC ] []
          , img 
            [ class "top"
            , style [ ("padding", "0px 5px") ]
            , src "graphics/CC.badge.png"
            , alt "Creative Commons badge"
            ] []
          , text "Only"
          ]
        ]
      , li [] [ text "List mode:" ]
      , let
          disable = not (model.viewMode == Designs && model.mainDesign == Nothing)
        in
          li []
          [ fieldset []
            [ radio "Small" (SwitchTo Design.Small)  (model.designMode == Design.Small) disable
            , text " "
            , radio "Large" (SwitchTo Design.Medium) (model.designMode == Design.Medium) disable
            ]
          ]
      ]
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
            [ li [] [ text "Upload!" ]
            , li [] [ a [href (makeUri "#user" [user.name, "0"])] [text "My uploads" ]]
            , li [] [ a [ onClick LogoutClick, href "#" ] [ text "Logout" ]]
            ]
          ]
        Nothing ->
          Html.map LoginMsg (Login.view model.loginform)
    ]
  , div [ id "CFAcontent" ]
    ( case model.viewMode of
        Designs ->
        ( case model.mainDesign of
            Nothing ->
              viewDesigns model
            Just design ->
              let
                vc = Design.ViewConfig Design.Large model.user
              in
                [ Html.map DesignMsg (Design.view vc design) ]
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
                [ case model.mainDesign of
                    Just design ->
                      table [class "welcometable"]
                        [ tr []
                          [ td [class "thumbcell"] 
                            [ a [href ("#design/" ++ (toString design.designid))] 
                              [ img [ class "image", src design.thumblocation, alt "design thumbnail"] []]
                            ]
                          ]
                        , tr []
                          [ td [] 
                            [ text "The Gallery's newest contributor is: "
                            , a [href (makeUri "#user" [design.owner, "0"])] [text design.owner]
                            , text ", whose latest piece is called "
                            , b [] [text design.title]
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
              List.map ((Design.view vcfg) >> (Html.map DesignMsg))
                                model.designList.designs
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
      _ -> Cmd.none

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
  Json.Decode.at ["commentid"] Json.Decode.int
      
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

decodeDesign : Json.Decode.Decoder Design.Design
decodeDesign =
   Json.Decode.at ["design"] Design.decodeDesign

getDesigns : String -> Int -> Int -> Cmd Msg
getDesigns query start count =
  let
    url = String.join "/" ["http://localhost:5000", query, 
                           toString(start), toString(count)]
  in
    Http.send NewDesigns (get url decodeDesigns)

decodeDesigns : Json.Decode.Decoder DesignList
decodeDesigns = 
  Json.Decode.map5 DesignList
    (Json.Decode.at ["designs"] (Json.Decode.list Design.decodeDesign))
    (Json.Decode.at ["prevlink"] Json.Decode.string)
    (Json.Decode.at ["nextlink"] Json.Decode.string)
    (Json.Decode.at ["thislink"] Json.Decode.string)
    (Json.Decode.at ["count"]    Json.Decode.int)

      
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
  Json.Decode.at ["userinfo"] User.decodeUser

logoutUser : Cmd Msg
logoutUser = 
  let
    url = "http://localhost:5000/logout"
  in
    Http.send LogoutUser (post url Http.emptyBody decodeUserLogout)

decodeUserLogout : Json.Decode.Decoder Bool
decodeUserLogout =
  Json.Decode.at ["logout_success"] Json.Decode.bool

getCfdg : Int -> Cmd Msg
getCfdg id =
  let
    url = "http://localhost:5000/data/cfdg/" ++ (toString id)
  in
    Http.send (ReceiveCfdg id) (Http.getString url)

getCfdgfromDesign : Design.Design -> Cmd Msg
getCfdgfromDesign design =
  getCfdg design.designid

getComments : Int -> Cmd Msg
getComments id =
  let
    url = "http://localhost:5000/comments/" ++ (toString id)
  in
    Http.send NewComments (get url decodeComments)

decodeComments : Json.Decode.Decoder (List Comment.Comment)
decodeComments =
  Json.Decode.at ["comments"] (Json.Decode.list Comment.decodeComment)

getTitle : String -> Cmd Msg
getTitle title =
  let
    url = "http://localhost:5000/titleindex/" ++ (Http.encodeUri title)
  in
    Http.send GotTitleIndex (get url decodeTitleIndex)

decodeTitleIndex : Json.Decode.Decoder Int
decodeTitleIndex = 
  Json.Decode.at ["index"] Json.Decode.int

getTags : Cmd Msg
getTags = 
  let
    url = "http://localhost:5000/tags"
  in
    Http.send GotTags (get url decodeTags)

decodeTags : Json.Decode.Decoder (List TagInfo)
decodeTags =
  Json.Decode.at ["tags"] (Json.Decode.list decodeTagInfo)

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
    (Json.Decode.at ["users"] (Json.Decode.list User.decodeMiniUser))
    (Json.Decode.at ["prevlink"] Json.Decode.string)
    (Json.Decode.at ["nextlink"] Json.Decode.string)
    (Json.Decode.at ["thislink"] Json.Decode.string)
    (Json.Decode.at ["count"]    Json.Decode.int)
      
