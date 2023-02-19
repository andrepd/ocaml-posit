open! Util.Prelude

open Posit__Impl

module P6_0 = Make(struct let nbits = 6 let es = 0 end)
module P6_1 = Make(struct let nbits = 6 let es = 1 end)
module P6_2 = Make(struct let nbits = 6 let es = 2 end)
module P16_0 = Make(struct let nbits = 16 let es = 0 end)
module P16_4 = Make(struct let nbits = 16 let es = 4 end)
module P16_12 = Make(struct let nbits = 16 let es = 12 end)
module P62 = Make(struct let nbits = 62 let es = 2 end)

module type SIG = module type of P6_0



let posit_range (module P : SIG) = 
  let open QCheck in
  let corner_cases = P.[zero; one; minus_one; nar; min_posit; neg min_posit; max_posit; neg max_posit] in
  let a,b = min_max (P.nar) (P.max_posit) in
  let range = int_range a b in
  range
  |> set_gen (Gen.graft_corners (get_gen range) corner_cases ()) (* Quite nice *)
  |> QCheck.map P.of_bits ~rev:Fun.id
  (* Though, how does this play with [QCheck.pair]? Do we generate them in 
     parallel, or all combinations? Looking at the source it seems they 
     generate in parallel, which is not good *)

let quire_range (module P : SIG) = 
  let open QCheck in
  let corner_cases = P.Quire.[q_zero; q_nar; max_value; Z.one; neg max_value; neg Z.one] in
  let gen = 
    let open Gen in
    let num_bytes = (P.Quire.quire_nbits-1) / Sys.int_size in
    let rem = (P.Quire.quire_nbits-1) mod Sys.int_size in
    Seq.ints 0 |> Seq.take num_bytes |> Seq.fold_left (fun acc i -> 
      let byte = Random.bits () in
      let offset = i * Sys.int_size in
      Z.(acc + (of_int byte lsl offset))
    ) Z.zero
    |> (fun acc -> 
      if rem = 0 then acc else
      let byte = Random.full_int ((1 lsl rem) - 1) in
      let offset = num_bytes * Sys.int_size in
      Z.(acc + (of_int byte lsl offset))
    )
    |> return
  in
  let gen = Gen.graft_corners gen corner_cases () in
  make gen ~print:Z.to_string 



let encode_decode (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> P.(encode @ decode x) = x)

let decode_interpret (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> 
    let decoded = P.decode x in
    let interpreted = P.interpret x in
    match decoded, interpreted with
    | `Zero, `Zero -> true
    | `Nar, `Nar -> true
    | `Num x, `Num y -> 
      let x = List.fold_left Q.mul Q.one [
        (if x.exp >= 0 then Q.mul_2exp else Q.div_2exp) Q.one (Int.abs x.exp);
        Q.of_ints x.frac P.frac_denom;
      ] in
      let y = List.fold_left Q.mul Q.one [
        (if y.sign then Q.one else Q.minus_one);
        (let exp = (y.regime lsl P.es) + y.exponent in (if exp >= 0 then Q.mul_2exp else Q.div_2exp) Q.one (Int.abs exp));
        Q.add Q.one (Q.of_ints y.fraction P.frac_denom);
      ] in
      Q.equal x y
    | _ -> false
  )

(* let%test "r_e_of_exp" = 
  let module P = P16_4 in
  P.r_e_to_exp (-5) *)

let s_f1 (module P : SIG) = 
  QCheck.(pair (bool) (int_bound (P.frac_denom - 1))),
  (fun (s,f) -> Poly.(P.(s_f_of_frac @ s_f_to_frac s f) = (s,f)))

let s_f2 (module P : SIG) = 
  let uncurry f (x,y) = f x y in
  let range_p = QCheck.int_range (P.frac_denom) ((P.frac_denom lsl 1) - 1) in
  let range_n = QCheck.int_range (-((P.frac_denom lsl 1) - 1)) (-P.frac_denom) in
  QCheck.(choose [range_p; range_n]),
  (fun x -> P.(uncurry s_f_to_frac @ s_f_of_frac x) = x)

let of_q_to_q (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> P.(of_q @ to_q x) = x)

let of_string_to_string (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> P.(of_string @ to_string x = x))

let neg_ (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> (* if P.is_nar x then P.neg x = x else *) P.neg x = P.of_q @ Q.neg @ P.to_q x)



let quire_neg (module P : SIG) = 
  (quire_range (module P)),
  (* (fun x -> Q.compare (Q.neg @ P.Quire.to_q x) (P.Quire.to_q @ P.Quire.neg x) = 0) *)
  (fun x -> P.eq (P.neg @ P.Quire.to_p x) (P.Quire.to_p @ P.Quire.neg x))

let quire_of_q_to_q (module P : SIG) = 
  (quire_range (module P)),
  (fun x -> Z.equal P.Quire.(of_q @ to_q x) x)

let quire_of_p_to_p (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> P.eq P.Quire.(to_p @ of_p x) x)

let quire_of_p_to_q (module P : SIG) = 
  (posit_range (module P)),
  (fun x -> Q.compare P.Quire.(to_q @ of_p x) (P.to_q x) = 0)

let quire_dot_product (module P : SIG) = 
  QCheck.(list @ pair (posit_range (module P)) (posit_range (module P))), 
  fun l -> 
    let x,y = List.split l in
    (* let a = List.fold_left2 (fun acc x y -> Q.(acc + P.to_q x * P.to_q y)) Q.zero x y |> P.of_q in
    let b = P.dot_prod_list x y in
    P.eq a b *)
    let a = List.fold_left2 (fun acc x y -> Q.(acc + P.to_q x * P.to_q y)) Q.zero x y in
    let b = List.fold_left2 (fun acc x y -> P.Quire.add_mul x y acc) P.Quire.empty x y |> P.Quire.to_q in
    Q.compare a b = 0



(** Ensure a binary function on posits matches the results when lifted to a binary function on [Zarith.Q]. *)
let lift2 (module P : SIG) (fp : P.t -> P.t -> P.t) (fq : Q.t -> Q.t -> Q.t) = 
  QCheck.(pair (posit_range (module P)) (posit_range (module P))),
  (fun (x,y) -> fp x y == P.of_q (fq P.(to_q x) P.(to_q y)))

let lift2' (module P : SIG) (fp : P.t -> P.t -> bool) (fq : Q.t -> Q.t -> bool) = 
  QCheck.(pair (posit_range (module P)) (posit_range (module P))),
  (fun (x,y) -> fp x y == fq P.(to_q x) P.(to_q y))

(* let eq (module P : SIG) = 
  lift2' (module P) P.eq Q.(=)

let ne (module P : SIG) = 
  lift2' (module P) P.ne Q.(<>) *)

let gt (module P : SIG) = 
  lift2' (module P) P.gt (* Q.(>) *) (fun x y -> Q.compare x y > 0)

let ge (module P : SIG) = 
  lift2' (module P) P.ge (* Q.(>=) *) (fun x y -> Q.compare x y >= 0)

let lt (module P : SIG) = 
  lift2' (module P) P.lt (* Q.(<) *) (fun x y -> Q.compare x y < 0)

let le (module P : SIG) = 
  lift2' (module P) P.le (* Q.(<=) *) (fun x y -> Q.compare x y <= 0)

let add (module P : SIG) = 
  lift2 (module P) P.add Q.(+)

let sub (module P : SIG) = 
  lift2 (module P) P.sub Q.(-)

let mul (module P : SIG) = 
  lift2 (module P) P.mul Q.( * )

let div (module P : SIG) = 
  lift2 (module P) P.div Q.(/)

(** Turn a qcheck property test into an exhaustive check *)
let exhaustive1 f (module P : SIG) = fun () -> 
  let prop = snd @ f (module P : SIG) in
  for x = P.nar to P.max_posit do
    Alcotest.(check bool) "" true (prop x)
  done

let exhaustive2 f (module P : SIG) = fun () -> 
  let prop = snd @ f (module P : SIG) in
  for x = P.nar to P.max_posit do
    for y = P.nar to P.max_posit do
      Alcotest.(check bool) "" true (prop (x,y))
    done
  done




let () =
  let open Alcotest in
  (* Test cases *)
  let q ?name = test_case (Option.value name ~default:"") `Quick in
  let s ?name = test_case (Option.value name ~default:"") `Slow in
  (* QCheck properties *)
  let qcheck ?name ?count (g,f) = QCheck_alcotest.to_alcotest @ QCheck.Test.make ?name ?count g f in
  let scheck ?name ?count (g,f) = QCheck_alcotest.to_alcotest ~long:true @ QCheck.Test.make ?name ?count g f in

  run ~verbose:false ~tail_errors:(`Limit 30) "Posit" [
    (* Manual cases *)
    "is_zero", [
      q (fun () -> check bool "" P6_1.(is_zero (of_bits 0b000000)) true);
    ]; 
    "is_nar", [
      q (fun () -> check bool "" P6_1.(is_nar (of_bits 0b100000)) true);
    ]; 
    "sign", [
      q (fun () -> check bool "" P6_1.(interpret_regular (of_bits 0b000010)).sign true);
      q (fun () -> check bool "" P6_1.(interpret_regular (of_bits 0b100010)).sign false);
    ]; 
    "regime", [
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b000011)).regime (-3));
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b100001)).regime 4);
    ]; 
    "exponent", [
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011111)).exponent 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011110)).exponent 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011100)).exponent 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011101)).exponent 1);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b010101)).exponent 1);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b010001)).exponent 0);
    ]; 
    "fraction", [
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011111)).fraction 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011110)).fraction 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011100)).fraction 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011101)).fraction 0);
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b010101)).fraction (0b01 lsl (P6_1.frac_width - 2)));
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b010001)).fraction (0b01 lsl (P6_1.frac_width - 2)));
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b010010)).fraction (0b10 lsl (P6_1.frac_width - 2)));
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b011001)).fraction (0b1 lsl (P6_1.frac_width - 1)));
      q (fun () -> check int "" P6_1.(interpret_regular (of_bits 0b010000)).fraction (0b00 lsl (P6_1.frac_width - 2)));
    ]; 
    "zero", [
      q (fun () -> check string "" P16.(to_string zero) "0");
    ]; 
    "one", [
      q (fun () -> check string "" P16.(to_string one) "1");
    ]; 
    "minus_one", [
      q (fun () -> check string "" P16.(to_string minus_one) "-1");
    ]; 
    "min_posit", [
      q (fun () -> check (testable Q.pp_print Q.(=)) "" P16.(to_q min_posit) Q.(div_2exp one P16.max_exp));
    ]; 
    "max_posit", [
      q (fun () -> check (testable Q.pp_print Q.(=)) "" P16.(to_q max_posit) Q.(mul_2exp one P16.max_exp));
    ]; 
    "rounding", (let f (module P: SIG) a b () = check string "" P.(to_string ~format:`Dec ~precision:20 @ of_string a) b in [
      q ~name:"pi" @ f (module P8)    "3.14159"  "3.25";
      q ~name:"pi" @ f (module P8)   "-3.14159" "-3.25";
      q ~name:"pi" @ f (module P6_1)  "3.14159"  "3";
      q ~name:"pi" @ f (module P6_1) "-3.14159" "-3";
      q ~name:"+ nearest frac down" @ f (module P6_1)  "3.15"  "3";
      q ~name:"- nearest frac down" @ f (module P6_1) "-3.15" "-3";
      q ~name:"+ nearest frac even" @ f (module P6_1)  "3.25"  "3";
      q ~name:"- nearest frac even" @ f (module P6_1) "-3.25" "-3";
      q ~name:"+ nearest frac up"   @ f (module P6_1)  "3.35"  "3.5";
      q ~name:"- nearest frac up"   @ f (module P6_1) "-3.35" "-3.5";
      q ~name:"+ nearest exp down" @ f (module P6_1)  "127"  "64";
      q ~name:"- nearest exp down" @ f (module P6_1) "-127" "-64";
      q ~name:"+ nearest exp even" @ f (module P6_1)  "128"  "64";
      q ~name:"- nearest exp even" @ f (module P6_1) "-128" "-64";
      q ~name:"+ nearest exp up"   @ f (module P6_1)  "129"  "256";
      q ~name:"- nearest exp up"   @ f (module P6_1) "-129" "-256";
      q ~name:"+ max_posit" @ f (module P6_1) "256" "256";
      q ~name:"+ max_posit" @ f (module P6_1) "3948281201" "256";
      q ~name:"- max_posit" @ f (module P6_1) "-256" "-256";
      q ~name:"- max_posit" @ f (module P6_1) "-3948281201" "-256";
      q ~name:"+ min_posit" @ f (module P6_1) "1/256" "0.00390625";
      q ~name:"+ min_posit" @ f (module P6_1) "0.00000000002383" "0.00390625";
      q ~name:"- min_posit" @ f (module P6_1) "-1/256" "-0.00390625";
      q ~name:"- min_posit" @ f (module P6_1) "-0.00000000002383" "-0.00390625";
      (* TODO: same for P63, to catch eventual corner cases *)
    ]); 

    (* Exhaustive/qcheck property testing *)
    "IntExt", [
      qcheck ~name:"~-" (QCheck.(tup3 (int) (int_bound 1) (int_bound 1)), (fun x -> Poly.(IntExt.(-(-x)) = x)));
      qcheck ~name:"~-" (QCheck.int, (fun x -> Poly.(IntExt.(-(x,0,0)) = (-x,0,0))));
    ]; 
    "r_e_*", [
    ];
    "s_f_*", [
      qcheck ~name:"P16_4" ~count:10_000 @ s_f1 (module P16_4);
      qcheck ~name:"P16_4" ~count:10_000 @ s_f2 (module P16_4);
      q (fun () -> let f x = x lsl (P6_1.frac_width - 3) in check int "" (P6_1.s_f_to_frac true  (f 0b101)) (f 0b01_101));
      q (fun () -> let f x = x lsl (P6_1.frac_width - 3) in check int "" (P6_1.s_f_to_frac false (f 0b101)) (f 0b10_011));
      q (fun () -> let f x = x lsl (P6_1.frac_width - 3) in check int "" (P6_1.s_f_to_frac false (f 0b000)) (f 0b11_000));
    ];
    "encode/decode", [
      q ~name:"P8"    @ exhaustive1 encode_decode (module P8);
      q ~name:"P16_0" @ exhaustive1 encode_decode (module P16_0);
      q ~name:"P16_4" @ exhaustive1 encode_decode (module P16_4);
      q ~name:"P16_12" @ exhaustive1 encode_decode (module P16_12);
      qcheck ~name:"P32" ~count:20_000_000 @ encode_decode (module P32);
      qcheck ~name:"P62" ~count:20_000_000 @ encode_decode (module P62);
      qcheck ~name:"P63" ~count:20_000_000 @ encode_decode (module P63);
    ];
    "decode/interpret", [
      q ~name:"P8"    @ exhaustive1 decode_interpret (module P8);
      q ~name:"P16_0" @ exhaustive1 decode_interpret (module P16_0);
      q ~name:"P16_4" @ exhaustive1 decode_interpret (module P16_4);
      q ~name:"P16_12" @ exhaustive1 decode_interpret (module P16_12);
      qcheck ~name:"P32" ~count:20_000_000 @ decode_interpret (module P32);
      qcheck ~name:"P62" ~count:20_000_000 @ decode_interpret (module P62);
      qcheck ~name:"P63" ~count:20_000_000 @ decode_interpret (module P63);
    ];
    "of_q/to_q", [
      qcheck ~name:"P16_0"  ~count:10_000 @ of_q_to_q (module P16_0);
      qcheck ~name:"P16_4"  ~count:10_000 @ of_q_to_q (module P16_4);
      qcheck ~name:"P16_12" ~count:10_000 @ of_q_to_q (module P16_12);
      qcheck ~name:"P63"    ~count:10_000 @ of_q_to_q (module P63);
    ];
    "of_string/to_string", [
      q ~name:"P6_2"  @ exhaustive1 of_string_to_string (module P6_2);
      q ~name:"P8"    @ exhaustive1 of_string_to_string (module P8);
      q ~name:"P16_0" @ exhaustive1 of_string_to_string (module P16_0);
      q ~name:"P16_4" @ exhaustive1 of_string_to_string (module P16_4);
      q ~name:"P16_12" @ exhaustive1 of_string_to_string (module P16_12);
      qcheck ~name:"P32" ~count:1_000_000 @ of_string_to_string (module P32);
      qcheck ~name:"P63" ~count:1_000_000 @ of_string_to_string (module P63);
    ];
    "lt", [
      q ~name:"P6_0" @ exhaustive2 lt (module P6_0);
      qcheck ~name:"P16_4" ~count:1_000_000  @ lt (module P16_4);
      qcheck ~name:"P63"   ~count:10_000_000 @ lt (module P63);
    ];
    "le", [
      q ~name:"P6_0" @ exhaustive2 le (module P6_0);
      qcheck ~name:"P16_4" ~count:1_000_000  @ le (module P16_4);
      qcheck ~name:"P63"   ~count:10_000_000 @ le (module P63);
    ];
    "gt", [
      q ~name:"P6_0" @ exhaustive2 gt (module P6_0);
      qcheck ~name:"P16_4" ~count:1_000_000  @ gt (module P16_4);
      qcheck ~name:"P63"   ~count:10_000_000 @ gt (module P63);
    ];
    "ge", [
      q ~name:"P6_0" @ exhaustive2 ge (module P6_0);
      qcheck ~name:"P16_4" ~count:1_000_000  @ ge (module P16_4);
      qcheck ~name:"P63"   ~count:10_000_000 @ ge (module P63);
    ];
    "neg", [
      qcheck ~name:"P6_0" ~count:4096  @ neg_ (module P6_0);
      qcheck ~name:"P6_1" ~count:4096  @ neg_ (module P6_1);
      qcheck ~name:"P6_2" ~count:4096  @ neg_ (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ neg_ (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ neg_ (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ neg_ (module P63);
    ];
    "add", [
      q ~name:"P6_0" @ exhaustive2 add (module P6_0);
      q ~name:"P6_1" @ exhaustive2 add (module P6_1);
      q ~name:"P6_2" @ exhaustive2 add (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ add (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ add (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ add (module P63);
      (* qcheck ~name:"P32"   ~count:10_000_000 @ add (module P32); *)
    ];
    "sub", [
      q ~name:"P6_0" @ exhaustive2 sub (module P6_0);
      q ~name:"P6_1" @ exhaustive2 sub (module P6_1);
      q ~name:"P6_2" @ exhaustive2 sub (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ sub (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ sub (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ sub (module P63);
    ];
    "mul", [
      q ~name:"P6_0" @ exhaustive2 mul (module P6_0);
      q ~name:"P6_1" @ exhaustive2 mul (module P6_1);
      q ~name:"P6_2" @ exhaustive2 mul (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ mul (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ mul (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ mul (module P63);
    ];
    "div", [
      q ~name:"P6_0" @ exhaustive2 div (module P6_0);
      q ~name:"P6_1" @ exhaustive2 div (module P6_1);
      q ~name:"P6_2" @ exhaustive2 div (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ div (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ div (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ div (module P63);
    ];

    "quire_neg", [
      qcheck ~name:"P6_0"  ~count:100_000    @ quire_neg (module P6_0);
      qcheck ~name:"P6_1"  ~count:100_000    @ quire_neg (module P6_1);
      qcheck ~name:"P6_2"  ~count:100_000    @ quire_neg (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ quire_neg (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ quire_neg (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ quire_neg (module P63);
    ];
    "quire_of_q/to_q", [
      qcheck ~name:"P6_0"  ~count:100_000    @ quire_of_q_to_q (module P6_0);
      qcheck ~name:"P6_1"  ~count:100_000    @ quire_of_q_to_q (module P6_1);
      qcheck ~name:"P6_2"  ~count:100_000    @ quire_of_q_to_q (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ quire_of_q_to_q (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ quire_of_q_to_q (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ quire_of_q_to_q (module P63);
    ];
    "quire_of_p/to_p", [
      q ~name:"P6_0" @ exhaustive1 quire_of_p_to_p (module P6_0);
      q ~name:"P6_1" @ exhaustive1 quire_of_p_to_p (module P6_1);
      q ~name:"P6_2" @ exhaustive1 quire_of_p_to_p (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ quire_of_p_to_p (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ quire_of_p_to_p (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ quire_of_p_to_p (module P63);
    ];
    "quire_of_p/to_q", [
      q ~name:"P6_0" @ exhaustive1 quire_of_p_to_q (module P6_0);
      q ~name:"P6_1" @ exhaustive1 quire_of_p_to_q (module P6_1);
      q ~name:"P6_2" @ exhaustive1 quire_of_p_to_q (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000  @ quire_of_p_to_q (module P16_4);
      qcheck ~name:"P62"   ~count:10_000_000 @ quire_of_p_to_q (module P62);
      qcheck ~name:"P63"   ~count:10_000_000 @ quire_of_p_to_q (module P63);
    ];
    "quire_dot_product", [
      qcheck ~name:"P6_0"  ~count:100_000 @ quire_dot_product (module P6_0);
      qcheck ~name:"P6_1"  ~count:100_000 @ quire_dot_product (module P6_1);
      qcheck ~name:"P6_2"  ~count:100_000 @ quire_dot_product (module P6_2);
      qcheck ~name:"P16_4" ~count:1_000_000 @ quire_dot_product (module P16_4);
      qcheck ~name:"P62"   ~count:2_000_000 @ quire_dot_product (module P62);
      qcheck ~name:"P63"   ~count:2_000_000 @ quire_dot_product (module P63);
    ];
  ]
