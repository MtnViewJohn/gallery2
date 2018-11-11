module User exposing
  ( User
  , canModify
  , canTag
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
  , isTagger : Bool
  , lastLogin : Time.Posix
  , joinedOn : Time.Posix
  , numPosts : Int
  , numLogins : Int
  , unseen : Int
  , notify : Bool
  , defaultccURI : String
  }

type alias MiniUser = 
  { name : String
  , joinedOn : Time.Posix
  , numPosts : Int
  }



decodeUser : JD.Decoder User
decodeUser =
    JD.succeed User
        |> JPipe.required "username" (JD.string)
        |> JPipe.required "email" (JD.string)
        |> JPipe.required "admin" (JD.bool)
        |> JPipe.required "tagger" (JD.bool)
        |> JPipe.required "lastlogin" (JD.map int2Time JD.int)
        |> JPipe.required "joinedon" (JD.map int2Time JD.int)
        |> JPipe.required "numposts" (JD.int)
        |> JPipe.required "numlogins" (JD.int)
        |> JPipe.optional "unseen" (JD.int) 0
        |> JPipe.required "notify" (JD.bool)
        |> JPipe.required "ccURI" (JD.string)

decodeMiniUser : JD.Decoder MiniUser
decodeMiniUser =
    JD.succeed MiniUser
        |> JPipe.required "username" (JD.string)
        |> JPipe.required "joinedon" (JD.map int2Time JD.int)
        |> JPipe.required "numposts" (JD.int)




canModify: String -> Maybe User -> Bool
canModify owner loggedUser =
  case loggedUser of
    Nothing -> False
    Just user ->
      user.isAdmin || (owner == user.name)

canTag : String -> Maybe User -> Bool
canTag owner loggedUser =
  case loggedUser of
    Nothing -> False
    Just user ->
      user.isTagger || (owner == user.name)



