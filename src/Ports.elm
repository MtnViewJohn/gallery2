-- Ports.elm
port module Ports exposing (..)

type alias FilePortData =
  { fileid   : String
  , contents : String
  , filename : String
  }

port fileSelected : String -> Cmd msg
port utf8FileSelected : String -> Cmd msg

port fileContentRead : (FilePortData -> msg) -> Sub msg

port scrollToElement : String -> Cmd msg

port scrolledToElement : (String -> msg) -> Sub msg

port checkVisible : String -> Cmd msg

port isVisible : (String -> msg) -> Sub msg

port pageTitle : String -> Cmd a 
