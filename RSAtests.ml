open OUnit2
open RSA

let pub, priv = generate_public_private_keys "adit" "nandini"
let pub1, priv1 = generate_public_private_keys "!@#$%^&*()" "{}:|:<>?,./;}"
let pub2, priv2 = generate_public_private_keys "rocky" "rocky"
let pub3, priv3 = generate_public_private_keys "" "cs"
let encrypted = encrypt_message "BOLDERZ9!" pub3
let encrypted1 = encrypt_message "1234" pub3

let tests = "test suite for rsa" >::: [
  "same keys"  >:: (fun _ -> assert_equal (generate_public_private_keys "adit"
  "nandini") (generate_public_private_keys "nandini" "adit"));
  "numbers"  >:: (fun _ -> assert_equal (generate_public_private_keys "fletcher1235"
  "arshi09876") (generate_public_private_keys "arshi09876" "fletcher1235"));
  "special"  >:: (fun _ -> assert_equal (generate_public_private_keys "!@#$%^&*()"
  "{}|:::<>?}") (generate_public_private_keys "{}|:::<>?}" "!@#$%^&*()"));
  "empty"  >:: (fun _ -> assert_equal (generate_public_private_keys ""
  "hello_world") (generate_public_private_keys "hello_world" ""));
  "same keys - small names"  >:: (fun _ -> assert_equal
                                     (generate_public_private_keys "1"
  "2") (generate_public_private_keys "2" "1"));
  "case sensitive"  >:: (fun _ -> assert_equal true (
                            (generate_public_private_keys "A" "b") !=
                            (generate_public_private_keys "a" "b")));
  "fail" >:: (fun _ -> assert_raises (Invalid_argument "Message not coded")
                 (fun () -> (decrypt_message "ajshdvjch" priv)));
  "fail2" >:: (fun _ -> assert_raises
                  (Invalid_argument "Message encrypted with wrong public key")
                  (fun () -> (decrypt_message "1234 1234 1234" priv)));
  "case coded"  >:: (fun _ -> assert_equal true
                        ("secret message" != (encrypt_message
                                                "secret message" pub1)));
  "case coded2"  >:: (fun _ -> assert_equal true
                         ("!@#$" != (encrypt_message "!@#$" pub)));
  "case coded2"  >:: (fun _ -> assert_equal true
                         ("252525" != (encrypt_message "252525" pub)));
  "decrypt encrypt1"  >:: (fun _ ->
      assert_equal ("hello wooooo") (decrypt_message
                              (encrypt_message "hello wooooo" pub) priv));
  "decrypt encrypt2"  >::
  (fun _ -> assert_equal ("123456789!@#$%^&*()")
      (decrypt_message (encrypt_message "123456789!@#$%^&*()" pub2) priv2));
  "decrypt encrypt3"  >:: (fun _ -> assert_equal ("gwaLA64!")
                              (decrypt_message (encrypt_message
                                                  "gwaLA64!" pub) priv));
  "empty"  >:: (fun _ -> assert_equal ""
                              (decrypt_message (encrypt_message
                                                  "" pub) priv));
  "ecr empty" >:: (fun _ -> assert_equal "" (encrypt_message "" pub2));
  "encrypt same"  >:: (fun _ -> assert_equal (encrypted)
                          (encrypt_message "BOLDERZ9!" pub3));
  "encrypt same"  >:: (fun _ -> assert_equal (encrypted1)
                          (encrypt_message "1234" pub3));
  "dec empty" >:: (fun _ -> assert_equal "" (decrypt_message "" priv2));
]

let _ = run_test_tt_main tests
