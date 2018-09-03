open Core
open Async
open RSA

let stdout_writer = Lazy.force Writer.stdout
let print_message s = Writer.write stdout_writer s
let recipient = ref ""
let user = ref ""
let port = 8080
let public_key = ref (1,1)
let private_key = ref (1,1)
let in_chat = ref false

(* [get_msg r] waits for a message to be received on the reader [r]
 * and then prints it to the terminal. It also determines whether the incoming
 * message needs to be decrypted and handles decryption if necessary *)
let rec get_msg r =
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok msg ->
      let default_length = String.length ">>You are now messaging " in
      let default_length2 = String.length ">>You are logged in as " in

      if msg <> "" then
        if String.length msg > default_length2 &&
             ((String.sub msg 0 default_length2) = ">>You are logged in as ")
          then let _ = user := String.sub msg default_length2
                                 (String.length msg - default_length2) in
          (print_message msg; print_message "\n"; return r >>= get_msg)
        else if String.length msg > default_length &&
                  ((String.sub msg 0 default_length) = ">>You are now messaging ")
          then let _ = recipient := String.sub msg default_length (
                                    String.length msg - default_length);
                           in_chat := true in
          let result = generate_public_private_keys !user !recipient in
          let _ = public_key := fst result; private_key := snd result in
                (print_message msg; print_message "\n"; return r >>= get_msg)
        else if (not !in_chat) then
                (print_message msg; print_message "\n"; return r >>= get_msg)
        else if (String.length msg > 2 &&
                  (String.sub msg 0 2 = ">>" || String.sub msg 0 2 = "//")) then
                (print_message msg; print_message "\n"; return r >>= get_msg)
        else if (String.length msg > 7 &&
                  (String.sub msg 0 7 = "[Error]")) then
                (print_message msg; print_message "\n"; return r >>= get_msg)
        else if (String.length msg > 13 &&
                  (String.sub msg 0 13 = "[System bell]")) then
                (print_message msg; print_message "\n"; return r >>= get_msg)
        else
          let usr_length = String.length !recipient + 1 in
          let msg' = String.sub msg (usr_length)
                       (String.length msg - usr_length) in
          let msg'' = decrypt_message msg' !private_key in
          (print_message (!recipient ^ ": " ^ msg'');
           print_message "\n"; return r >>= get_msg)
      else return r >>= get_msg

(* [send_msg w] reads in what a user types on the terminal and then sends
 * the message to the server using writer [w]. It encrypts messages which
 * which are not built-in commands before sending them. *)
let rec send_msg w =
  Reader.read_line (Lazy.force Reader.stdin) >>= function
  | `Eof -> return ()
  | `Ok msg ->
      if (String.length msg > 2 && String.sub msg 0 2 = "//") || (not !in_chat)
        then if (msg = "//leave" && (not !in_chat)) then
          let _ = (print_message "[Error] Must be in a chat to leave.\n") in
          send_msg w
        else let _ = Writer.write w msg; Writer.write_char w '\n'; in
        if (msg = "//quit") then exit 0
        else if msg = "//leave" then let _ = in_chat := false in send_msg w
        else send_msg w
      else if msg = "" then send_msg w
      else
        let msg' = encrypt_message msg !public_key in
        Writer.write w msg'; Writer.write_char w '\n';
        send_msg w

(* [start_chat r w] creates deferred objects for both sending and recieving
 * messages, allowing either to happen.
 * [r] is the reader object
 * [w] is the writer object *)
let start_chat r w =
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok msg -> print_message msg; print_message "\n";
               ignore (send_msg w);
               get_msg r

let client () =
  (Tcp.connect (Tcp.Where_to_connect.of_host_and_port
                  {host = "localhost"; port})) >>= (fun (_, r, w) ->
                                                      start_chat r w)

(* starts up the client and scheduler *)
let () =
  ignore (client ());
  never_returns (Scheduler.go ())
