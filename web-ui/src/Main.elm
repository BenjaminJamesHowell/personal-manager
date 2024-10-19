module Main exposing (main)

import State
import View
import Browser

main : Platform.Program () State.Model State.Msg
main =
    Browser.element
        { init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.view
        }

