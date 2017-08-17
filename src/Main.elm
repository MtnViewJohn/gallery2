import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onSubmit, onInput)
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


type alias Model =
  { user : Maybe User
  , loginform : Login.Model
  , limitCC : Bool
  , authorLookup : String
  , designLookup : Int
  , mainDesign : Maybe Design.Design
  , designList : List Design.Design
  , currentLocation : Navigation.Location
  , viewMode : ViewMode
  , showLinkBar : Bool
  }

initModel : Navigation.Location -> (Model, Cmd Msg)
initModel loc = 
  (Model Nothing Login.initModel False "" 0 Nothing [] loc Designs False, loginSession)


-- URL PARSING


type Route
  = Home
  | DesignID Int
  | Author String Int Int
  | AuthorInit String
  | Newest Int Int
  | NewestInit
  | Oldest Int Int
  | OldestInit
  | Title Int Int
  | TitleInit
  | TitleIndex String
  | Popular Int Int
  | PopularInit
  | RandomDes Int Int Int
  | RandomInit Int
  | RandomSeed



route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map Home top
    , Url.map DesignID (Url.s "design" </> int)
    , Url.map Author (Url.s "user" </> Url.string </> int </> int)
    , Url.map AuthorInit (Url.s "user" </> Url.string)
    , Url.map Newest (Url.s "newest" </> int </> int)
    , Url.map NewestInit (Url.s "newest")
    , Url.map Oldest (Url.s "oldest" </> int </> int)
    , Url.map OldestInit (Url.s "oldest")
    , Url.map Title (Url.s "title" </> int </> int)
    , Url.map TitleInit (Url.s "title")
    , Url.map TitleIndex (Url.s "titleindex" </> Url.string)
    , Url.map Popular (Url.s "popular" </> int </> int)
    , Url.map PopularInit (Url.s "popular")
    , Url.map RandomDes (Url.s "random" </> int </> int </> int)
    , Url.map RandomInit (Url.s "random" </> int)
    , Url.map RandomSeed (Url.s "random")
    ]




-- UPDATE


type Msg
  = LoginClick 
  | LogoutClick
  | CCcheck Bool
  | NewURL Navigation.Location
  | LoginMsg Login.Msg
  | DesignMsg Design.Msg
  | LookupName
  | LookupDesign
  | AuthorText String
  | DesignText String
  | NewSeed Int
  | NewDesign (Result Http.Error Design.Design)
  | NewDesigns (Result Http.Error (List Design.Design))
  | NewUser (Result Http.Error User.User)
  | SessionUser (Result Http.Error User.User)
  | LogoutUser (Result Http.Error Bool)
  | ReceiveCfdg Int (Result Http.Error String)
  | NewComments (Result Http.Error (List Comment.Comment))
  | GotTitleIndex (Result Http.Error Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoginClick ->
      (model, loginUser model.loginform)
    LogoutClick ->
      (model, logoutUser)
    CCcheck cc ->
      ({ model | limitCC = cc }, Cmd.none)
    NewURL loc ->
      case Url.parseHash route loc of
        Nothing ->
          ({model | currentLocation = loc}, Cmd.none)
        Just route ->
          case route of
            Home ->
              ({model | mainDesign = Nothing, 
                        designList = [], 
                        currentLocation = loc}, Cmd.none)
            DesignID id ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs },
                getDesign id)
            Author name start count ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns ("by/" ++ name) start count)
            AuthorInit name ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns ("by/" ++ name) 0 50)
            Newest start count ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns "newest" start count)
            NewestInit ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns "newest" 0 50)
            Oldest start count ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns "oldest" start count)
            OldestInit ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns "oldest" 0 50)
            Title start count ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = True },
                getDesigns "title" start count)
            TitleInit ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = True },
                getDesigns "title" 0 50)
            TitleIndex title ->
              (model, getTitle title)
            Popular start count ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns "popular" start count)
            PopularInit ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns "popular" 0 50)
            RandomDes seed start count ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns ("random/" ++ (toString seed)) start count)
            RandomInit seed ->
              ({model | currentLocation = loc, mainDesign = Nothing, 
                viewMode = Designs, showLinkBar = False },
                getDesigns ("random/" ++ (toString seed)) 0 50)
            RandomSeed ->
              (model, Random.generate NewSeed (Random.int 1 1000000000))
    NewSeed seed ->
      (model, Navigation.newUrl ("#random/" ++ (toString seed)))
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
        Nothing -> (model, Cmd.none)    -- shouldn't happen
        Just des ->
          let
            (newDesign, maybeAction) = Design.update dmsg des
          in
            (model, Cmd.none)           -- not implemented yet
    LookupName ->
      (model, Navigation.newUrl ("#user/" ++ model.authorLookup))
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
          ({model | mainDesign = Nothing, designList = designs}, Cmd.none)
        Err error ->
          ({model | designList = []}, Cmd.none)
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
    ReceiveCfdg id cfdgResult ->    -- ignoring id for now
      case cfdgResult of
        Ok cfdgText ->
          case model.mainDesign of
            Just oldDesign ->
              let
                newDesign = Design.setCfdg cfdgText oldDesign 
              in
                ({model | mainDesign = Just newDesign}, Cmd.none)
            Nothing ->            -- Shouldn't happen
              (model, Cmd.none)   -- Just drop it
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
    GotTitleIndex indexResult ->
      case indexResult of
        Ok index ->
          (model, Navigation.newUrl ("#title/" ++ (toString index) ++ "/50"))
        Err _ ->
          (model, Cmd.none)





-- VIEW



designString: Int -> String
designString des =
  if des < 1 then
    ""
  else
    toString des

makeIndexLink : Char -> List (Html Msg)
makeIndexLink c =
  [ a [class "letterref", href ("#titleindex/" ++ (String.fromChar c))] [text (String.fromChar c)], text " "]

view : Model -> Html Msg
view model =
  div []
  [ div [ id "CFAcolumn" ]
    [ h5 [] [ text "Gallery Tools:" ]
    , ul []
      [ li [] [ a [href "#newest"] [text "Newest" ]]
      , li [] [ a [href "#oldest"] [text "Oldest" ]]
      , li [] [ a [href "#title"]  [text "Title" ]]
      , li [] [ a [href "#popular"] [text "Popular" ]]
      , li [] [ a [href "#random"] [text "Random" ]]
      , li [] [ text "People" ]
      , li [] [ text "Tags" ]
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
            , li [] [ text "My uploads" ]
            , li [] [ a [ onClick LogoutClick, href "#" ] [ text "Logout" ]]
            ]
          ]
        Nothing ->
          Html.map LoginMsg (Login.view model.loginform)
    ]
  , div [ id "CFAcontent" ]
    ( case model.mainDesign of
        Nothing ->
          if List.isEmpty model.designList then
            [ text "Nothing to show" ]
          else
            (if model.showLinkBar then
              [ a [class "letterref", href "#title"] [text "all"], text " "
              ] ++
              List.concat (List.map makeIndexLink (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
              ++ [hr [][]] 
            else
              []
            )
            ++
            let
              vc = Design.ViewConfig Design.Small model.user
            in
              (List.map ((Design.view vc) >> (Html.map DesignMsg))
                model.designList)
        Just design ->
          let
            vc = Design.ViewConfig Design.Large model.user
          in
            [ Html.map DesignMsg (Design.view vc design) ]
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

decodeDesigns : Json.Decode.Decoder (List Design.Design)
decodeDesigns = 
    Json.Decode.at ["designs"] (Json.Decode.list Design.decodeDesign)

      
loginUser : Login.Model -> Cmd Msg
loginUser lmodel =
  let
    url = String.join "/" ["http://localhost:5000/login", lmodel.user, 
                           lmodel.password, if lmodel.remember then "1" else "0"]
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
    url = "http://localhost:5000/titleindex/" ++ title
  in
    Http.send GotTitleIndex (get url decodeTitleIndex)

decodeTitleIndex : Json.Decode.Decoder Int
decodeTitleIndex = 
  Json.Decode.at ["index"] Json.Decode.int
      

