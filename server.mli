open Core
open Async

(* A [Server] connects to a client and sets up their username. It then
 * connect two clients who wish to chat sends messages between them. *)

(* [server ()] starts up a server
 * example: let () =
 *            ignore (server ());
 *            never_returns (Scheduler.go ()) *)
val server : unit -> (Socket.Address.Inet.t, int) Async.Tcp.Server.t
                      Async_kernel.Deferred.t
