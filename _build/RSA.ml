(* array of primes *)
let primes = ref [2797; 733; 6007; 4957; 2719; 2081; 4271; 5639; 2749; 2663;
659; 1523; 2399; 6073; 2609; 2969; 2153; 1951; 1483; 1009; 5417; 2441; 3527;
3989; 4877; 5449; 3083; 3881; 3769; 1129; 3793; 4357; 5701; 3449; 1609; 5953;
1217; 1069; 3631; 751; 3767; 5867; 4051; 2137; 1231; 3761; 5171; 757; 3547;
643; 691; 1733; 1549; 2963; 5483; 4093; 3541; 773; 991; 5737; 4933; 3511; 4523;
1873; 4481; 997; 5851; 521; 3643; 5189; 4651; 797; 3041; 5927; 2917; 4703; 1789;
5279; 563; 3907; 1087; 5741; 4789; 947; 1051; 1553; 3583; 2063; 1093; 2039;
1193; 2777; 4519; 887; 3307; 1871; 3607; 5651; 577; 1427; 2909; 3203; 967;
2957; 4091; 3593; 4799; 1723; 4973; 2851; 2477; 3617; 1831; 677; 3191; 4349;
4513; 1783; 4283; 3299; 941; 4001; 3929; 1613; 4643; 3637; 5717; 2767; 5119;
3659; 809; 5569; 4861; 5381; 1373; 5281; 2447; 2689; 2207; 2131; 2971; 1399;
3331; 2551; 5441; 5849; 881; 3943; 4231; 2161; 5107; 4793; 5333; 6047; 4003;
3739; 4987; 5309; 599; 2351; 2753; 1301; 3259; 1949; 3019; 3319; 1987; 2003;
4127; 4391; 3709; 4363; 3323; 2729; 5749; 4013; 601; 3373; 1213; 5779; 4201;
2017; 727; 3677; 3061; 571; 1103; 2269; 2113; 2741; 6089; 3917; 5501; 4159;
3221; 3533; 5641; 1559; 3217; 2029; 1637; 4243; 4451; 1229; 4217; 1489; 3413;
3359; 769; 1321; 2861; 4019; 2473; 2221; 3931; 5231; 2389; 5443; 1283; 4999;
1753; 3407; 5039; 523; 1801; 4801; 1459; 4621; 3301; 1021; 2521; 2699; 2731;
683; 4111; 2897; 2791; 3137; 739; 1699; 1777; 5323; 5431; 4663; 2339; 4733;
3119; 4129; 4397; 2459; 4679; 761; 5653; 5647; 2503; 2069; 5683; 4817; 1471;
1567; 4561; 3671; 2903; 3313; 2027; 5081; 2833; 4723; 2693; 2377; 2531; 2357;
3023; 1583; 2819; 3253; 5981; 4909; 2437; 1201; 1847; 1249; 6029; 1493; 5479;
2111; 613; 5101; 5521; 1223; 1867; 3391; 4177; 5923; 1289; 3571; 4549; 811;
1601; 2371; 5903; 1487; 2647; 1619; 839; 2557; 1303; 3701; 3797; 701; 1811;
5557; 5209; 1669; 1171; 547; 4813; 607; 2087; 3089; 1823; 2053; 1181; 3457;
1297; 1259; 1033; 4337; 1759; 4759; 3923; 5419; 3803; 6091; 5783; 5857; 2383;
4079; 3529; 3049; 1877; 1741; 2887; 2677; 859; 1627; 4133; 929; 4021; 2659;
2333; 5791; 5669; 2341; 1931; 1447; 2593; 4903; 5471; 1319; 2089; 3719; 5021;
1151; 3539; 2591; 1423; 3623; 829; 6053; 863; 2843; 4027; 5939; 1907; 4153;
5531; 4507; 4211; 3499; 787; 2293; 1901; 4721; 5591; 641; 4297; 541; 631; 2617;
1709; 2179; 2687; 3947; 5227; 1787; 4673; 2707; 4441; 3673; 4373; 4919; 2939;
2381; 5303; 709; 2287; 4007; 3257; 1481; 2621; 5693; 2927; 3037; 1667; 1063;
4157; 3001; 3121; 2281; 3517; 1409; 617; 1453; 2129; 5113; 1013; 3187; 3557;
2579; 5503; 1721; 977; 1889; 1367; 5563; 4057; 5347; 3271; 4447; 6043; 2837;
1933; 821; 1621; 4259; 883; 2273; 5843; 971; 2347; 5807; 3371; 4409; 5399; 2711;
5413; 2417; 5273; 4967; 2393; 5011; 2539; 1429; 1579; 4327; 4463; 911; 2549;
2657; 3469; 3877; 5237; 2011; 5059; 4637; 2099; 1693; 3491; 919; 3467; 4583;
1123; 2311; 4253; 1433; 3461; 4969; 3833; 3821; 4457; 4099; 5827; 5881; 1061;
3329; 2801; 5351; 3347; 2251; 1571; 5407; 1697; 1187; 3229; 6079; 5051; 5507;
4289; 3847; 5261; 4783; 719; 2543; 1019; 2297; 1117; 5897; 2671; 2239; 2411;
5099; 857; 3109; 5077; 4493; 1109; 2879; 1307; 3919; 6067; 4951; 5861; 1973;
509; 5197; 1451; 4517; 4649; 3011; 5437; 4139; 3361; 1097; 5153; 983; 4751;
4219; 2213; 647; 4993; 3209; 4591; 1381; 2243; 1327; 1861; 1439; 3067; 5839;
1039; 5657; 1657; 4889; 1747; 4603; 619; 2467; 2309; 1979; 3167; 3613; 5813;
3343; 953; 1997; 4241; 3169; 653; 1031; 3079; 5087; 3697; 2803; 1091; 5659; 823;
4423; 1499; 5167; 3779; 5801; 3389; 5023; 4261; 1277; 3163; 661; 5147; 3727;
2267; 5393; 1531; 5009; 2789; 5879; 5581; 1361; 4229; 1607; 1999; 3863; 4937;
1291; 3691; 4943; 2141; 3733; 3251; 5743; 4597; 2203; 1279; 3911; 4691; 2857;
557; 4049; 3967; 1049; 1153; 4567; 1511; 1993; 827; 3889; 2143; 907; 2237; 5987;
5527; 1163; 4729; 4273; 5869; 853; 3559; 5711; 2713; 1237; 877; 3581; 6037;
4421; 1597; 5689; 1879; 5573; 587; 3433; 743; 3853; 4787; 2683; 6011; 5387;
1543; 3823; 5003; 4657; 4339; 4871; 593; 673; 4073; 2083; 5821; 569; 3851; 3463;
937; 4483; 2999; 4639; 1913; 2633; 4547; 5477; 5233; 2423; 4931; 5623; 5519;
5179; 4831; 2953; 3181; 5297; 1663]

let generate_primes_indices usr1 usr2 =
  let comb = if usr1 > usr2 then usr1^usr2 else usr2^usr1 in
  let hash1 = Hashtbl.hash comb in
  let len = List.length !primes - 1 in
  let pi = hash1 mod len in
  let qi = (pi + 1) mod len in
  let qi = if qi = pi then (qi+1) mod (len+1) else qi in
  pi, qi

(* [inverse a b] finds an integer d such that d*a [=] 1 (mod b), 0 <= d < b *)
let inverse a b =
  let rec inverse a b x y u v = (* Gets a d such that d*a [=] 1 (mod b) *)
    if !a = 0 then !x
    else
      let q = !b / !a in
      let r = !b mod !a in
      let m = !x - !u*q in
      let n = !y - !v*q in
      b := !a;
      a := r;
      x := !u;
      y := !v;
      u := m;
      v := n;
      inverse a b x y u v in
    let d = inverse (ref a) (ref b) (ref 0) (ref 1) (ref 1) (ref 0) in
    let rec get_in_bounds d b = (* Makes sure 0 <= d < b*)
      if d < 0 then get_in_bounds (d + b) b
      else if d >= b then get_in_bounds (d - b) b
      else d
    in get_in_bounds d b

(* [generate_public_private_keys usr1 usr2] returns randomly generated private
 * and public keys which is the same for the usernames combination. *)
let generate_public_private_keys usr1 usr2 =
  let pi,qi = generate_primes_indices usr1 usr2 in
  let p = List.nth !primes pi in
  let q = List.nth !primes qi in
  let n = p * q in
  let t = (p-1) * (q-1) in
  let len = List.length !primes in
  let start = (qi + 1) mod len in
  let rec find_e index=                     (*finds an e such that e is*)
    let guess = List.nth !primes index in   (*relatively prime to t*)
    if (t mod guess = 0 || guess=p || guess=q) then find_e (index + 1 mod len)
    else guess
  in let e = find_e start in
  let d = inverse e t in
  (n,e),(n,d)

(* [to_char_array str] is the char array corresponding [str]*)
let to_char_array str =
  let rec to_char_array str acc num =
    if num < 0 then acc
    else to_char_array str (str.[num]::acc) (num - 1)
  in to_char_array str [] (String.length str - 1)

(* [encrypt_char pub chr] encrypts the char [chr] using [pub] key*)
let encrypt_char (n,e) chr =
  let open Z in (* Avoids overflows for integers*)
  let e = Z.of_int e in
  let n = Z.of_int n in
  let num = Char.code chr in
  let zar_num = Z.of_int num in
  let exponentiated = Z.powm zar_num e n in
  let back_int = Z.to_int exponentiated in
  string_of_int back_int

(* [decrypt_char priv chr] decrypts the char [chr] using [priv] key*)
let decrypt_char (n,d) chr =
  let open Z in (* Avoids overflows for integers*)
  let e = Z.of_int d in
  let n = Z.of_int n in
  let num = int_of_string chr in
  let zar_num = Z.of_int num in
  let exponentiated = Z.powm zar_num e n in
  let back_int = Z.to_int exponentiated in
  String.make 1 (Char.chr back_int)

let encrypt_message msg pub =
  let char_arr = to_char_array msg in
  let arr = List.map (fun m -> encrypt_char pub m) char_arr in
  String.concat " " arr

let decrypt_message msg priv =
  if msg = "" then ""
  else
    try
      let msg = String.trim msg in
      let arr = String.split_on_char ' ' msg in
      let lst = List.map (fun m -> decrypt_char priv m) arr in
      String.concat "" lst
    with
    | Failure _ -> raise (Invalid_argument "Message not coded")
    | Invalid_argument _ ->
        raise (Invalid_argument "Message encrypted with wrong public key")
