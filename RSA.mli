(* [generate_!primes_indices usr1 usr2] generates 2 indices based on
 * [usr1] and [usr2] usernames unique to the usernames combination*)
val generate_public_private_keys :
                                string  -> string -> (int * int) * (int * int)

(* [encrypt_message msg pub] is the encrypted [msg] using [pub] key. *)
val encrypt_message : string -> int * int -> string

(* [decrypt_message msg priv] is the decrypted [msg] using [priv] key. *)
val decrypt_message : string -> int * int -> string
