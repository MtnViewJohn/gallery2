-- Ports.elm
port module Ports exposing (..)

port scrollToElement : String -> Cmd msg

port scrolledToElement : (String -> msg) -> Sub msg

port checkVisible : String -> Cmd msg

port isVisible : (String -> msg) -> Sub msg
