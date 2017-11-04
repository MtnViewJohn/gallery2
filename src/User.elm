module User exposing
  ( User
  , canModify
  , decodeUser
  , MiniUser
  , decodeMiniUser
  )


import Json.Decode as JD
import Json.Decode.Pipeline as JPipe
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
  , unseen : Int
  , notify : Bool
  , defaultccURI : String
  }

type alias MiniUser = 
  { name : String
  , joinedOn : Time.Time
  , numPosts : Int
  }



decodeUser : JD.Decoder User
decodeUser =
    JPipe.decode User
        |> JPipe.required "username" (JD.string)
        |> JPipe.required "email" (JD.string)
        |> JPipe.required "admin" (JD.bool)
        |> JPipe.required "lastlogin" (JD.map int2Time JD.int)
        |> JPipe.required "joinedon" (JD.map int2Time JD.int)
        |> JPipe.required "numposts" (JD.int)
        |> JPipe.required "numlogins" (JD.int)
        |> JPipe.optional "unseen" (JD.int) 0
        |> JPipe.required "notify" (JD.bool)
        |> JPipe.required "ccURI" (JD.string)

decodeMiniUser : JD.Decoder MiniUser
decodeMiniUser =
    JPipe.decode MiniUser
        |> JPipe.required "username" (JD.string)
        |> JPipe.required "joinedon" (JD.map int2Time JD.int)
        |> JPipe.required "numposts" (JD.int)




canModify: String -> Maybe User -> Bool
canModify owner loggedUser =
  case loggedUser of
    Nothing -> False
    Just user ->
      user.isAdmin || (owner == user.name)


