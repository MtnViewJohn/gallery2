module Login exposing
  ( Model
  , initModel
  , Action

  , Msg
  , update
  , fail
  , view
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onSubmit)
import String exposing (isEmpty, trimLeft)

-- MODEL

type alias Model = { user : String, password : String, remember : Bool, message : String }


initModel : Model
initModel = Model "" "" False ""



type Action = LoginAttempt

-- UPDATE

type Validate
    = Ok
    | NotOk String

validate: Model -> Validate
validate model =
  if isEmpty model.user then
    NotOk "User name is empty"
  else if isEmpty model.password then
    NotOk "Password is empty"
  else
    Ok 


type Msg
    = UserText String
    | PassText String
    | RememberCheck Bool
    | LoginClick

update : Msg -> Model -> (Model, Maybe Action)
update msg model =
  case msg of
    UserText name ->
      ({ model | user = trimLeft(name) }, Nothing)
    PassText pass ->
      ({ model | password = trimLeft(pass) }, Nothing)
    RememberCheck remember ->
      ({ model | remember = remember }, Nothing)
    LoginClick ->
      case validate model of
        Ok ->
          ({ model | message = "" }, Just LoginAttempt)
        NotOk err ->
          ({ model | message = err }, Nothing)

fail : String -> Model -> Model
fail errMsg model =
  { model | message = errMsg }

-- VIEW


view : Model -> Html Msg
view model =
  Html.form [ id "loginform", onSubmit LoginClick ] 
  [ h5 [] [ text "Login:" ]
  , ul [ class "loginform" ] 
    [ li [] 
      [ text "User:"
      , input [type_ "text", placeholder "Name", onInput UserText, size 12,
               name "username", id "username", value model.user] [ ]
      ]
    , li [] 
      [ text "Pass:"
      , input [type_ "password", placeholder "Password", onInput PassText, size 12,
               name "password", id "password", value model.password] [ ]
      ]
    , li [] 
      [ label []
        [ input [ type_ "checkbox", onCheck RememberCheck, checked model.remember ] []
        , text " Remember me"
        ]
      ]
    , li [] [ input [ type_ "submit", value "Login" ] [ ] ]
    , li [ style [("color", "red")]] [ text model.message ]
    ]
  , ul []
    [ li [] [ a [ href "../phpbb/ucp.php?mode=sendpassword" ] [ text "Lost password?" ] ]
    , li [] [ a [ href "../phpbb/ucp.php?mode=register" ] [ text "Register" ] ]
    ]
  ]