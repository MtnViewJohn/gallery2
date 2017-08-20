module User exposing
  ( User
  , canModify
  , decodeUser
  , MiniUser
  , decodeMiniUser
  )


import Json.Decode
import Json.Decode.Pipeline
import Time
import GalleryUtils exposing (..)

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

type alias MiniUser = 
  { name : String
  , joinedOn : Time.Time
  , numPosts : Int
  }



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

decodeMiniUser : Json.Decode.Decoder MiniUser
decodeMiniUser =
    Json.Decode.Pipeline.decode MiniUser
        |> Json.Decode.Pipeline.required "username" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "joinedon" (Json.Decode.map int2Time Json.Decode.int)
        |> Json.Decode.Pipeline.required "numposts" (Json.Decode.int)




canModify: String -> Maybe User -> Bool
canModify owner loggedUser =
  case loggedUser of
    Nothing -> False
    Just user ->
      user.isAdmin || (owner == user.name)


