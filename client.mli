open Core
open Async
open RSA

(* A [Client] connects to the server and other clients through the server. It starts chats and sends
   and receives messages. *)

(* [client ()] connects to the server and then starts up a chat
 * example: let () =
 *            ignore (client ());
 *            never_returns (Scheduler.go ())
 *)
val client: unit -> unit Deferred.t
