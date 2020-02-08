port module Main exposing (main)

{-| Imports -------------------------------------------------------------------}
import Lisp.Parser as P
import Lisp.Transpiler as T
import Result.Extra as Result

{-| Ports ---------------------------------------------------------------------}
port console_i : (String -> msg) -> Sub msg
port console_o : (String, String) -> Cmd msg

type EvalResult
  = Success
  | Failure

send : (EvalResult, String) -> Cmd msg
send (result, output) =
  case result of
    Success ->
      console_o ( "success", output )

    Failure ->
      console_o ( "failure", output )


{-| Main ----------------------------------------------------------------------}
main : Program () () Msg
main =
  Platform.worker
    { init = \_ -> ( (), Cmd.none )
    , update = \msg _ -> ( (), update msg )
    , subscriptions = \_ -> console_i InputReceived
    }


{-| Update --------------------------------------------------------------------}
type Msg
  = InputReceived String

update : Msg -> Cmd Msg
update msg =
  case msg of
    InputReceived input ->
      P.run input
        |> Result.map T.run
        |> Result.unpack (\_ -> Tuple.pair Failure "") (\o -> Tuple.pair Success o)
        |> send
