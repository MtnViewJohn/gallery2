module Login exposing
  ( Model
  , initModel

  , Msg
  , update
  , view
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onSubmit)
import String exposing (isEmpty, trimLeft)
import GalleryUtils exposing (..)

-- MODEL

type alias Model = { user : String, password : String, remember : Bool }


initModel : Model
initModel = Model "" "" False




-- UPDATE


type Msg
    = UserText String
    | PassText String
    | RememberCheck Bool

update : Msg -> Model -> Model
update msg model =
  case msg of
    UserText name ->
      { model | user = trimLeft(name) }
    PassText pass ->
      { model | password = trimLeft(pass) }
    RememberCheck remember ->
      { model | remember = remember }

-- VIEW


view : Model -> String -> Html Msg
view model message =
  let
    url = makeUri "#login" [model.user, model.password, if model.remember then "1" else "0"]
  in
  Html.form [ id "loginform", action url, method "post"] 
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
        [ input [ type_ "checkbox", name "rememberme", onCheck RememberCheck, checked model.remember ] []
        , text " Remember me"
        ]
      ]
    , li [] [ input [ type_ "submit", value "Login" ] [ ] ]
    , li [ style "color" "red"] [ text message ]
    ]
  , ul []
    [ li [] [ a [ href "../phpbb/ucp.php?mode=sendpassword" ] [ text "Lost password?" ] ]
    , li [] [ a [ href "../phpbb/ucp.php?mode=register" ] [ text "Register" ] ]
    ]
  ]