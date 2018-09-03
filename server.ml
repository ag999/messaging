open Core
open Async

let port = 8080

let current_users = ref []
let users_to_w = ref []
let unsent_msgs = ref []
let currently_talking = ref []
let blocked = ref []

(* [generate_id id] takes in a user's [id], adds it to the user list and unsent
 * message list, then returns a boolean indicating whether the id was already
 * present in the user list *)
let generate_id id =
  let curr_users = !current_users in
  try (let _ = List.find_exn curr_users ~f:(fun a -> a = id) in false)
  with | _ -> current_users := id::curr_users;
    unsent_msgs := List.Assoc.add !unsent_msgs
                     ~equal:(fun a a' -> a = a') id [];
    true

(* [clear_user id] removes the [id] from all lists. It's used when [id] quits
 * the application. *)
let clear_user id =
  current_users := List.filter ~f:(fun x -> not (x=id)) !current_users;
  users_to_w := List.Assoc.remove !users_to_w ~equal:(fun a a' -> a = a') id;
  currently_talking := List.filter ~f:(fun (a,b) -> not (a=id) && not (b=id))
                         !currently_talking

(* [display_online id w] prints the usernames of every other user who is
 * online except for [id] themselves and people who've blocked [id] using
 * writer [w] *)
let display_online id w =
  let other_users = List.filter ~f:(fun x -> not (x=id)) !current_users in

  (* [get_blocked_by] gets people that have blocked you *)
  let rec get_blocked_by block_list acc id =
    match block_list with
    | [] -> acc
    | (a,b)::t -> if (b=id) then (get_blocked_by t (a::acc) id)
                  else (get_blocked_by t acc id) in

  (* [filter_out] takes out out blocked in new_online_users *)
  let rec filter_out blocked new_online_users =
    match blocked with
    | [] -> new_online_users
    | h::t -> filter_out t
                (List.filter ~f:(fun x -> not (x=h)) new_online_users) in

  let blocked_by_list =  get_blocked_by !blocked [] id in
  let other_users = filter_out blocked_by_list other_users in

  let sorted_users = List.sort compare other_users in
  Writer.write_line w (">>Here are the current users: " ^
                       (List.fold_left sorted_users ~init:"" ~f:(fun acc elt ->
                          acc^"\n>>"^elt)))

(* [help_message w] prints the list of possible commands using writer [w] *)
let help_message w =
  Writer.write_line w (">>The list of possible commands are as follows:\n" ^
                       "//online: display users available to chat\n" ^
                       "//leave: leave the current chat\n" ^
                       "//block 'user_id': block 'user_id'\n" ^
                       "//unblock 'user_id': unblock 'user_id'\n" ^
                       "//list_blocked: list all users you have blocked\n" ^
                       "//quit: quit the application altogether\n")

(* [list_blocked w id] prints the list of users [id] has blocked using
 * writer [w] *)
let list_blocked w id =
  let rec get_blocked blocked_lst acc id =
  match blocked_lst with
  | [] -> acc
  | (a,b)::t -> if (a=id) then get_blocked t (b::acc) id
                else get_blocked t acc id in

  let blocked_usrs = get_blocked !blocked [] id in

  Writer.write_line w (">>Here are the blocked users: " ^
                       (List.fold_left blocked_usrs ~init:"" ~f:(fun acc elt ->
                          acc^"\n>>"^elt)))

(* [handle_builtin w msg id] checks if [mes] is a built-in command, e.g. it
 * starts with "//", and does the corresponding action for the given user [id] *)
let handle_builtin w mes id =
  let msg = String.strip mes in
  if msg = "//online" then
    let _ = display_online id w in
    true
  else if msg = "//leave" then
    let _ =
      let curr_recip =
        List.Assoc.find_exn !currently_talking ~equal:(fun a a' -> a = a') id in
        currently_talking := List.filter ~f:(fun (a,_) -> not (a=id))
                               !currently_talking;
        current_users := curr_recip::!current_users;
        current_users := id::!current_users;
        current_users := List.dedup_and_sort ~compare:(compare)
                           !current_users in
    true
  else if msg = "//help" then
    let _ = help_message w in
    true
  else if msg = "//quit" then
    let _ = clear_user id in
    true
  else if msg = "//list_blocked" then
    let _ = list_blocked w id in
    true
  else if (String.length msg > 7) && (String.sub msg 0 7) = "//block" then
    let recipient = String.strip (String.sub msg 7 ((String.length msg - 7))) in
    blocked := (id,recipient)::!blocked;
    true
  else if (String.length msg > 9) && (String.sub msg 0 9) = "//unblock" then
    let recipient = String.strip (String.sub msg 9 ((String.length msg - 9))) in
    blocked := List.filter ~f:(fun (a,b) -> not (a=id && b = recipient)) !blocked;
    true
  else if (String.length msg > 1) && (String.sub msg 0 2) = "//" then
    let _ = Writer.write_line w
              "[Error] Invalid command. Try //help for possible commands\n" in
    true
  else false

(* [user_added id] notifies other users when a user [id] comes online *)
let user_added id =
  let mapping = !users_to_w in
  let rec notify_everyone lst =
  match lst with
  | [] -> ()
  | (u,w)::t -> if u = id then notify_everyone t
                else
                  Writer.write_line w
                    ("[System bell] " ^ id ^ " joined the chatroom\n");
                  notify_everyone t
  in notify_everyone mapping

(* [get_msg cur_user w] gets all the messages that [cur_user] from the person
 * has received they are currently chatting with and sends it to them using
 * writer [w] *)
let rec get_msg cur_user w =
  try (ignore (List.find_exn !currently_talking ~f:(fun (a,_) -> a = cur_user));
    if Writer.is_closed w then return ()
    else (
    match (List.Assoc.find !unsent_msgs ~equal:(fun a a' -> a = a') cur_user) with
    | Some msg_list ->
        let curr_chatting = (List.Assoc.find_exn !currently_talking
                              ~equal:(fun a a' -> a = a') cur_user) in

        (* [write_helper lst acc] is used to check that messages in [lst] are
         * from the user who is currently being chatted with. Messages from
         * a different user are added to [acc] which is then added back to
         * [unsent_msgs] at the end. *)
        let rec write_helper lst acc =
          (match lst with
           | [] ->
               unsent_msgs := List.Assoc.remove !unsent_msgs
                                ~equal:(fun a a' -> a = a') cur_user;
               unsent_msgs := (cur_user, acc)::!unsent_msgs;
               (after (Core.sec 0.01)) >>= fun () -> get_msg cur_user w
           | msg::t ->
               let id = String.sub msg 0 (String.length curr_chatting) in
               if id = curr_chatting then
                 let _ = Writer.write_line w msg; Writer.write_char w '\n' in
                 write_helper t acc
               else write_helper t (msg::acc))
        in write_helper msg_list []
    | None -> failwith "ID should have been added to unsent msg list"))
  with | _ -> return ()

(* [send_msg r w recipient id] reads in a message using reader [r] and then
 * adds it to the unsent message list of [recipient]. It also checks if the
 * message was a built in command using [handle_builtin w msg id] *)
let rec send_msg r w recipient id =
  Reader.read_line r >>= fun mes ->
  match mes with
  | `Eof ->
      let _ = clear_user id in
      (try (ignore (List.Assoc.find_exn !currently_talking
                      ~equal:(fun a a' -> a = a') recipient);
         let recip_w = (List.Assoc.find_exn !users_to_w
                         ~equal:(fun a a' -> a = a') recipient) in
         Writer.write_line recip_w
           ("[System bell] User " ^ id ^ " has left the chatroom");
         return ()) with | _ -> return ())
  | `Ok msg ->
      if (not (handle_builtin w msg id)) then
      begin
        match (List.Assoc.find !unsent_msgs
                 ~equal:(fun a a' -> a = a') recipient) with
        | Some msg_list ->
            unsent_msgs := List.Assoc.remove !unsent_msgs
              ~equal:(fun a a' -> a = a') recipient;
            unsent_msgs := (recipient, msg_list@[(id ^ ": " ^ msg)])::!unsent_msgs;
            send_msg r w recipient id
        | None -> failwith "ID should have been added to unsent msg list"
      end
      else if msg = "//leave" then
        (try (ignore (List.Assoc.find_exn !currently_talking
                        ~equal:(fun a a' -> a = a') recipient);
              let recip_w = (List.Assoc.find_exn !users_to_w
                               ~equal:(fun a a' -> a = a') recipient) in
              Writer.write_line recip_w
                ("[System bell] User " ^ id ^ " has left the chatroom");
              chatter r w id) with | _ -> chatter r w id)
      else send_msg r w recipient id

(* [start_chat r w cur_user recipient] starts a chat between
 * [cur_user] and [recipient] using reader [r] and writer [w] *)
and start_chat r w cur_user recipient =
  Writer.write_line w (">>You are now messaging " ^ recipient ^ "\n");
  try (ignore (List.Assoc.find_exn !currently_talking
                 ~equal:(fun a a' -> a = a') recipient);
       currently_talking := ((cur_user,recipient)::!currently_talking);
       try (ignore (List.find_exn !currently_talking
                      ~f:(fun (a,b) -> a=recipient && b=cur_user));
            current_users :=
              List.filter ~f:(fun x -> not (x=recipient || x=cur_user))
                !current_users;
            ignore (get_msg cur_user w);
            send_msg r w recipient cur_user)
       with | _ -> ignore (get_msg cur_user w);
                   send_msg r w recipient cur_user)
  with | _ ->
    let recip_w = (List.Assoc.find_exn !users_to_w
                     ~equal:(fun a a' -> a = a') recipient) in
    Writer.write_line recip_w
      ("[System bell] " ^ cur_user ^ " is trying to message you\n");
    currently_talking := ((cur_user,recipient)::!currently_talking);
    try (ignore (List.find_exn !currently_talking
                   ~f:(fun (a,b) -> a=recipient && b=cur_user));
         current_users := List.filter
                            ~f:(fun x -> not (x=recipient || x=cur_user))
                            !current_users;
         ignore (get_msg cur_user w);
         send_msg r w recipient cur_user)
    with | _ -> ignore (get_msg cur_user w);
                send_msg r w recipient cur_user

(* [chatter r w] asks the client who they would like to message and starts a
 * chat between them and the recepient they ask for using reader [r] and
 * writer [w] *)
and chatter r w id =
  Writer.write_line w (">>Who would you like to message?\n");
  Reader.read_line r >>= function
  | `Eof ->
      let _ = clear_user id in
      return ()
  | `Ok recipient ->
      if (not (handle_builtin w recipient id)) then
        let recipient = String.lowercase recipient in
        if recipient = id then
          let _ = Writer.write_line w
                    ("[Error] You can't chat with yourself\n") in
          chatter r w id
        else
          try (ignore (List.find_exn !current_users ~f:(fun a -> a = recipient));
               try (ignore (List.find_exn !blocked
                              ~f:(fun (a,b) -> a = recipient && b =id));
                    Writer.write_line w ("[Error] Recipient unavailable\n");
                    chatter r w id)
               with | _ -> start_chat r w id recipient)
          with | _ ->
            Writer.write_line w
              ("[Error] Recipient unavailable\n"); chatter r w id
      else chatter r w id

(* [new_user r w] welcomes the user to the chat room and prompts them to
 * create a username. If the username input from [r] is an invalid username
 * it displays an error message prompts the user to enter a different name *)
let new_user r w =
  Writer.write_line w
    (">>Welcome to our chat room! Type //help for a list of possible commands\n");
  Writer.write_line w (">>What username do you want?\n");
  let rec create_user () =
    Reader.read_line r >>= function
    | `Eof -> return ""
    | `Ok id ->
        let id = String.lowercase id in
        if (not (handle_builtin w id "")) then
          if (String.contains id '/') then
            let _ = Writer.write_line w
                      ("[Error] '/' not allowed in user name\n") in
            create_user ()
          else if (String.contains id ' ') then
            let _ = Writer.write_line w
                      ("[Error] Spaces not allowed in user name\n") in
            create_user ()
          else if (String.contains id ':') then
            let _ = Writer.write_line w
                      ("[Error] ':' not allowed in user name\n") in
            create_user ()
          else if (String.length id = 0) then
            let _ = Writer.write_line w
                      ("[Error] Empty strings cannot be a username\n") in
            create_user ()
          else if (String.contains id '>') then
            let _ = Writer.write_line w
                      ("[Error] '>' not allowed in user name\n") in
            create_user ()
          else if (generate_id id) then
            let _ = user_added id in
            users_to_w := List.Assoc.add !users_to_w
                            ~equal:(fun a a' -> a = a') id w;
            Writer.write_line w (">>You are logged in as " ^ id);
            return id
          else
            let _ = Writer.write_line w
                      ("[Error] Username taken, please try a different name\n") in
            create_user ()
        else create_user ()
  in create_user () >>= fun id -> display_online id w; chatter r w id

let server () =
  Tcp.Server.create (Tcp.Where_to_listen.of_port port)
    ~on_handler_error:`Raise
    (fun _ reader writer -> new_user reader writer)

(* starts up the server and scheduler *)
let () =
  ignore (server ());
  never_returns (Scheduler.go ())
