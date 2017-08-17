module User exposing
  (
    User,
    canModify,
    decodeUser
  )


import Json.Decode
import Json.Decode.Pipeline
import Time

type alias User = 
  { name : String
  , email : String
  , isAdmin : Bool
  , lastLogin : Time.Time
  , joinedOn : Time.Time
  , numPosts : Int
  , numLogins : Int
  , notify : Bool
  , defaultccURI : String
  }

int2Time : Int -> Time.Time
int2Time i = 
  (toFloat i) * 1000.0


decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.Pipeline.decode User
        |> Json.Decode.Pipeline.required "username" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "email" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "admin" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "lastlogin" (Json.Decode.map int2Time Json.Decode.int)
        |> Json.Decode.Pipeline.required "joinedon" (Json.Decode.map int2Time Json.Decode.int)
        |> Json.Decode.Pipeline.required "numposts" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "numlogins" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "notify" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "ccURI" (Json.Decode.string)


canModify: String -> Maybe User -> Bool
canModify owner loggedUser =
  case loggedUser of
    Nothing -> False
    Just user ->
      user.isAdmin || (owner == user.name)