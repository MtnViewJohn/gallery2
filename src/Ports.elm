-- Ports.elm
port module Ports exposing (..)

type alias FilePortData =
  { fileid   : String
  , contents : String
  , filename : String
  }

port fileSelected : String -> Cmd msg

port fileContentRead : (FilePortData -> msg) -> Sub msg

port scrollToElement : String -> Cmd msg

port scrolledToElement : (String -> msg) -> Sub msg
