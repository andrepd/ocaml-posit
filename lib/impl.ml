open! Util.Prelude
let int_size = Sys.int_size

let dbg = false



(** Some bit-level functions on ocaml ints *)
module Bits = struct
  let clz = Ocaml_intrinsics.Int.count_leading_zeros

  (** Aux function: checks if bit i is set in x *)
  let[@inline] is_set i x = 
    assert (0 <= i && i < int_size); 
    x land (1 lsl i) <> 0

  (** Aux function: mask only i lowest bits *)
  let[@inline] lowest i x = 
    assert (0 <= i && i < int_size); 
    x land ((1 lsl i) - 1)

  (** Aux function: sign-extend x from n to machine size *)
  let[@inline] sign_extend n x = 
    let diff = int_size - n in
    (x lsl diff) asr diff

  (** Aux function: true iff x and y have different signs *)
  let[@inline] unequal_signs x y = 
    bool_of_int @ (x lxor y) lsr (int_size-1)

  (** As [x < 0], but very slightly faster *)
  let[@inline] is_neg x = 
    bool_of_int (x lsr (int_size-1))

  (** Negate x if b is false *)
  let[@inline] neg_if_not b x = 
    if b then x else lnot x
end

(** Represents an integer value extended with a round and sticky bit. 
    Hopefully this all gets inlined in a reasonably nice way! 

    The idea is that two extra bits suffice to determine whether the rounded 
    number is exact, inexact but smaller than number+½, inexact but greater 
    than number+½, or exactly number+½. 

    Imagine the number (e.g. the theoretical result of a calculation) is 
    extended to an infinite number of bits below the decimal point. The main 
    number is the result truncated to the precision of an int, the round bit 
    is the bit immediately after the main part, and the sticky bit is the 
    bitwise-or of all the bits below the round bit (i.e. 0 if and only if all 
    bits below round are 0). Then e.g.: 

      number r s
      101011 0 0  ← exactly 101011.000…
      101011 0 1  ← greater than 101011.000…, but smaller than 101011.100…
      101011 1 0  ← exactly 101011.100…
      101011 1 1  ← greater than 101011.100…, but smaller than 101100.000… 

    In posits, this is enough to determine whether to round down (if smaller 
    than number+½), up (if greater than number+½) or to nearest even (if 
    exactly equal to number+½) *)
module IntExt = struct
  (** IntExt representing an exact int. *)
  let[@inline] of_int x = (x,0,0)

  (** (Arithmetic) shift right: shift the lowest n bits of x into round and 
      sticky. Msb of the shifted out bits goes to round, the rest plus the 
      old round gets or'ed onto sticky. *)
  let[@inline] (asr) (x,r,s) n = 
    assert (n <= int_size);
    if n = 0 then (x,r,s) else
    let x' = x asr n in
    let r' = (x asr (n-1)) land 1 in
    let s' = s lor r lor (Bits.lowest (n-1) x) in
    (x', r', Bool.to_int (s' > 0))

  (** Shift left. The round bit gets recovered but the information in the 
      sticky bits is lost! We cannot know the value of the new round bit nor 
      the sticky bits. Therefore the returned value has the [n-1] lsb 
      undefined. *)
  let[@inline] (lsl) (x,r,s) n = 
    assert (n <= int_size);
    if n = 0 then x else
    let x' = (x lsl n) lor r in
    x'

  (** Negate the number, taking into account the round and sticky bits! 
      Two's complement of a the following number:

        xxxxxx r ssss

      where xxxxx is the number, r the round bit, and ssss the bits condensed 
      into the sticky bit.

      Two's complement is negate all bits and add 1. 

        - If ssss was all 0s (sticky bit = 0), it becomes all 1s. When we add 1, 
          it becomes all 0s and we carry the 1 into the round bit. 
          - If r was 0, negate becomes 1, add 1 (the carry from ssss) becomes 0. 
            There is a carry of 1 to add to the number xxxxxx. 
          - If r was 1, negate becomes 0, add 1 (the carry from ssss) becomes 1. 
            There is no carry to add to the number xxxxxx.
        - If ssss had at least one 1 (sticky bit = 1), it becomes a number with 
          at least one 0. When we add 1, it becomes a value with at least one 1, 
          but which does not overflow. So sticky bit becomes 1 and the rest is 
          unchanged just negated, since there is no carry into the rest. 

      Summary: sticky bit is unchanged (looks counter-intuitive at first glance!), 
      round bit is xor'ed with sticky bit, and the number is negated, adding 1 
      iff round and sticky were both 0. *)
  let[@inline] (~-) (x,r,s) = 
    let x' = (lnot x) + ((r lor s) lxor 1) in
    let r' = r lxor s in
    let s' = s in
    (x', r', s')

  (** Rounding procedure: look at the chopped off bits. If 
        xxxx r s
        .... 0 0
      then the number is exactly x, so we leave it. If 
        .... 0 1
      then the number is smaller than x+½, so we round down, to x. If 
        ...0 1 0
      then the number is exactly x+½, so we round to nearest even, which 
      is x. If
        ...1 1 0
      then the number is again exactly x+½, so we round to nearest even, 
      which is now x+1. If
        .... 1 1
      then the number is greater than x+½, so we round up by adding 1.  

      Conclusion, we add "round & sticky | round & (lsb of x)" to x. *)
  let[@inline] round (x,r,s) = 
    let l = x land 1 in
    let add = (r land s) lor (r land l) in
    x + add
end



(** A functor for a posit with an arbitrary size [nbits] and exponent size 
    [es]. It represents a posit value in an ocaml immediate, so [nbits] ≤ 
    [Sys.int_size]. *)
module Make (Params : sig val nbits : int val es : int end) = struct
  include Params

  let () = 
    if not (2 <= nbits && nbits <= int_size) then
      invalid_arg @ Printf.sprintf "nbits must be between 2 and %d (inclusive)" int_size
    else if not (es <= nbits) then
      invalid_arg @ "es must be at most nbits"

  type t = int

  (** Posit decoded into exponent and mantissa *)
  type decoded = {
    exp: int;   (** 2 ^ exp *)
    frac: int;  (** frac / frac_denom *)
  }

  (* frac represents a *signed, fixed-point* value, in the intervals ±[1;2[. 
     Therefore the decimal point is [frac_width] places from the right, 
     therefore it represents a fraction with numerator [frac] and denominator 
     [frac_denom] = 2^[frac_width]. *)
  let frac_width = int_size-2
  let frac_denom = 1 lsl frac_width

  (** The maximum/minimum exp (with exponent and regime bits) that can be 
      represented in this posit. *)
  let max_exp = (nbits-2) lsl es
  let min_exp = -max_exp

  (** Posit interpreted into the 4 human-readable fields *)
  type interpreted = {
    (** true → +, false → - *)
    sign: bool;
    (** 2 ^ (2^es × regime) *)
    regime: int;
    (** 2 ^ exponent *)
    exponent: int;
    (** 1 + fraction / frac_denom *)
    fraction: int;
  }

  (* Note this important invariant: at least 1 sign bit and 2 regime bits 
     means fraction has at most [nbits-3] significant bits. *)

  (** Translate between regime/exponent and exp *)
  let[@inline] r_e_of_exp exp = exp asr es, Bits.lowest es exp
  let[@inline] r_e_to_exp regime exponent = (regime lsl es) + exponent

  (** Translate between sign/fraction and frac *)
  let[@inline] s_f_of_frac frac = 
    assert (frac_denom <= abs frac);  (* Also covers the invalid case frac = Int.min_int *)
    frac >= 0, Bits.lowest frac_width (abs frac)

  let[@inline] s_f_to_frac sign fraction = 
    assert ((* fraction land 1 = 0 && *) fraction lsr frac_width = 0);
    let mask = (0b01 lsl frac_width) in
    if sign then mask lor fraction else -(mask lor fraction)

  let sign_extend = Bits.sign_extend nbits



  (* Basics *)

  let zero = 0
  let nar = sign_extend @ 1 lsl (nbits-1)
  (* TODO should nar be represented as Int.min_int regardless of nbits? This 
     would speed up neg *)

  let one       = sign_extend @ 0b01 lsl (nbits - 2)
  let minus_one = sign_extend @ 0b11 lsl (nbits - 2)

  (** Posits of minimum and maximum *absolute* value. *)
  let min_posit = 1  
  let max_posit = lnot nar

  let of_bits x = sign_extend x

  let is_zero x = x = zero
  let is_nar x = x = nar
  let is_real x = x <> nar

  (* type kind = Zero | Nar | Num *)



  (* Encode/decode *)

  (** Interpret the posit x into its human-readable components, the "textbook" 
      way. Assumes x is not zero or nar. *)
  let[@inline] interpret_regular x = 
    (* Get sign bit *)
    let sign = x > 0 in
    (* Interpret the absolute value of x *)
    let x = Int.abs x in
    (* Compute regime length and corresponding regime *)
    let regime_length, regime = 
      let first_bit = Bits.is_set (nbits - 2) x in
      let run_length = Bits.clz @ (if first_bit then lnot x else x) lsl (int_size - nbits + 1) in
      let run_length = if nbits <> int_size then min run_length (nbits-1) else run_length in (* Specialised here in the hopes compiler can figure it out *)
      let regime = if first_bit then run_length - 1 else -run_length in
      run_length, regime
    in
    (* After regime, there are this many leftover bits for exponent and fraction *)
    let leftover_bits = nbits - 1 - regime_length - 1 in
    let exponent, fraction = 
      if leftover_bits <= 0 then
        0, 
        0
      else if leftover_bits <= es then
        (Bits.lowest leftover_bits x) lsl (es - leftover_bits), 
        0
      else
        (Bits.lowest leftover_bits x) lsr (leftover_bits - es),
        (Bits.lowest (leftover_bits - es) x) lsl (frac_width - (leftover_bits - es))
    in
    {sign; regime; exponent; fraction}

  let interpret x = 
    if is_zero x then
      `Zero
    else if is_nar x then 
      `Nar
    else
      `Num (interpret_regular x)

  (* Decodes the posit x into binary exponent and signed "mantissa", the 
     efficient way. Assumes x is not zero or nar. *)
  let[@inline] decode_regular x = 
    (* This optimised decoding routine does not convert a number into its 
       2s-complement absolute value for processing. It's sometimes not trivial 
       why specific substeps hold. *)

    (* Shift out the meaningless bits (no-op iff nbits = int_size) *)
    let junk_length = int_size - nbits in
    let x = x lsl junk_length in
    
    (* Decode the regime. x xor'ed with itself shifted by 1 will give us two 
       things:

       * The MSB is sign_bit ⊕ regime_bit, and therefore will be 0 if the 
         regime is to be interpreted as a string of 0s, and vice-versa 
         (remember negative posits are interpreted as their twos-complement)

       * The remaining bits will be 0 during the run of 0s or 1s, and 1 in the 
         final bit of that run: think for example
             11110…
           ⊕ 11100…
           = 00010… 
         meaning shifting this left by 1 and counting leading zeroes will give 
         us regime length-1 (we call this regime_length_raw). 

       Then, the regime value is related to the regime length as follows: if the 
       "sign bit of the regime" (msb of the regime bits, i.e. whether we have a 
       run of 0s or 1s) is 

       * 0: regime = -regime_length = -(regime_length_raw+1) = lnot regime_length_raw
       * 1: regime = regime_length-1 = regime_length_raw 

       We use efficient bit-twiddling to accomplish this. *)
    let sign = not @ Bits.is_neg x in
    let x_xor = x lxor (x lsl 1) in
    let regime_length_raw = Bits.clz (x_xor lsl 1) in
    (* let regime_mask = x_xor asr int_size in
    let regime = regime_length_raw lxor (lnot regime_mask) in *)
    let regime_neg = Bits.is_neg x_xor in
    let regime = Bits.neg_if_not regime_neg regime_length_raw in

    (* Now we extract the exponent bits. There is: 1 bit sign, regime_raw+1 
       bits regime, 1 bit regime terminator, so the exponent MSB is 
       regime_raw+3 bits from the left. We want to shift it into es bits from 
       the right. Therefore: 
         1. Shift left by "regime_length_raw+3" to clear the regime bits
         2. Negate if negative
         3. Shift right by "int_size-es" to put in its place *)
    let x' = (x lsl regime_length_raw) lsl 3 in
    let exponent = (Bits.neg_if_not sign x') lsr (int_size - es) in

    (* And the fraction bits. We need to
         1. Shift left by "es" to clear the exponent bits
         2. Shift right by 2 to place them in the "fraction" part of frac 
            (remember the first two bits are the hidden bits: 0b01 = 1. for 
            positive and 0b10 = -2 for negative). *)
    let x'' = x' lsl es in
    let fraction = x'' lsr 2 in

    (* Finally, we just build exp from regime and exponent and frac from 
       fraction and sign *)
    let exp = (regime lsl es) + exponent in
    let frac = (Int.min_int lsr (Bool.to_int sign)) + fraction in
    {frac; exp}

  let decode x = 
    if is_zero x then
      `Zero
    else if is_nar x then 
      `Nar
    else
      `Num (decode_regular x)

  (* Careful! The {exp;frac} pair may not necessarily correspond exactly to a posit (i.e. it may 
     not be the output of any [decode_regular] call). Exp may lie outside the representable range 
     and exp or frac may have more precision than can be represented. *)
  (* If frac should be interpreted as having bits set beyond its lsb, s should be set *)
  (* However, frac is assumed to be normalised such that 1 ≤ abs frac < 2 *)
  (* Explicit round bit never needed: at least 1 lsb of frac is always chopped off (int_size-3 
     is the maximum, and 2 are for the hidden bits, that leaves 1). *)
  let[@inline] encode_regular ?(s=0) {exp; frac} = 
    assert (frac = 0 || frac_denom <= abs frac);  (* Also covers frac = Int.min_int *)
    if frac = 0 then zero else
    let regime, exponent = r_e_of_exp exp in
    let sign, fraction = s_f_of_frac frac in
    (* Corner cases: saturates at |x| ≥ max_posit and |x| ≤ min_posit *)
    if exp >= max_exp then if sign then max_posit else -max_posit else (* Bias! |x| ≥ max_posit = 1.0×2^max_exp ⇔ exp ≥ min_exp *)
    if exp <  min_exp then if sign then min_posit else -min_posit else (* but   |x| ≤ min_posit = 1.0×2^min_exp ⇔ exp < min_exp *)
    (* From regime calculate regime *)
    let regime_bit, regime_length = 
      if regime >= 0 then
        1, regime+1
      else   
        0, -regime
    in
    (* This is the number of leftover bits for exponent and fraction *)
    let leftover_bits = nbits - 1 - regime_length - 1 in
    assert (regime_length < nbits && leftover_bits < nbits);
    (* Compute string of 0s or 1s and shift to the right place *)
    let regime_bits = 
      if regime_bit = 0 then
        let mask = 1 in
        mask lsl leftover_bits
      else
        let mask = (1 lsl regime_length) - 1 in
        mask lsl (leftover_bits+1)
    in
    (* Using the functions in IntExt, it is now easy to ensure rounding is correct *)
    let number = 
      let r = 0 in
      if leftover_bits <= 0 then (* exponent and fraction all chopped off *)
        let _,r,s = IntExt.((fraction,r,s) asr frac_width) in
        let _,r,s = IntExt.((exponent,r,s) asr es        ) in
        regime_bits, r, s
      else if leftover_bits < es then (* exponent partiallly and fraction all chopped off *)
        let _,r,s = IntExt.((fraction,r,s) asr frac_width          ) in
        let _,r,s = IntExt.((exponent,r,s) asr (es - leftover_bits)) in
        let exponent_bits = exponent lsr (es - leftover_bits) in
        (regime_bits lor exponent_bits), r, s
      else (* exponent preserved and fraction partially chopped off *)
        let _,r,s = IntExt.((fraction,r,s) asr (frac_width - (leftover_bits - es))) in
        let exponent_bits = exponent lsl (leftover_bits - es) in
        let fraction_bits = fraction lsr (frac_width - (leftover_bits - es)) in
        (regime_bits lor exponent_bits lor fraction_bits), r, s
    in
    let number = if sign then number else IntExt.(-number) in
    IntExt.round number

  let encode = function
    | `Zero -> zero
    | `Nar -> nar
    | `Num x -> encode_regular x



  (* Printing *)

  (* For testing and printing, this helper function converts a posit to an 
     exact [Zarith.Q] rational. *)
  let to_q x = 
    match decode x with
    | `Zero -> Q.zero
    | `Nar -> Q.undef
    | `Num {exp; frac} -> 
      let exp = (if exp >= 0 then Q.mul_2exp else Q.div_2exp) Q.one (Int.abs exp) in
      let frac = Q.of_ints frac frac_denom in
      Q.(exp * frac)

  (* Not exported *)
  let to_string_dbg x = 
    let s = Printf.sprintf in
    match interpret x with
    | `Zero -> "0"
    | `Nar -> "NaR"
    | `Num {sign; regime; exponent; fraction} -> 
      let useed = Z.shift_left Z.one (1 lsl es) in
      let sign = if sign then '+' else '-' in
      let fraction = Q.(one + of_ints fraction frac_denom) in
      s "%c %a^%d × 2^%d × %a" sign Z.sprint useed regime exponent Q.sprint fraction

  let to_string ?format ?precision x = 
    let max_figures = 
      (* Enough precision to ensure the result, if converted back to posit, 
         is rounded to the same posit. *)
      let default = 
        if nbits <= 8 then 2 + 1 else 
        if nbits <= 16 then 5 + 1 else 
        if nbits <= 32 then 10 + 1 else 
        21 + 1
      in
      Option.value precision ~default
    in
    match decode x with
    | `Zero -> "0"
    | `Nar -> "NaR"
    | `Num {exp; frac} -> 
      let sign = if frac >= 0 then "" else "-" in
      let frac = abs frac in
      let num, den = 
        if exp >= 0 then 
          Z.(of_int frac lsl exp), Z.(of_int frac_denom)
        else
          Z.(of_int frac), Z.(of_int frac_denom lsl (Int.neg exp))
      in
      (* Extract digits of integral part *)
      let digits = if Z.Compare.(num < den) then "" else Z.(to_string @ num / den) in
      let exponent_pos = String.length digits in
      (* Extract digits of decimal part (optimisation: skip this number of 
         leading 0s at least = underestimation of log10(den)/log10(num)). *)
      let skipped_zeroes = max ((Z.numbits den - Z.numbits num - 1) * 3 / 10) 0 in
      let num = Z.(num * (of_int 10 ** skipped_zeroes)) in
      assert (skipped_zeroes = 0 || Z.Compare.(num < den));
      (* Extract digits one at a time, by multiplying the remainder of num/den 
         until the remainder is 0 or we hit the number of maximum digits. *)
      let rec loop buf exponent_neg num den = 
        if Z.(equal num zero) || Buffer.length buf = max_figures then 
          exponent_neg
        else
          let num = Z.(num * of_int 10) in
          let digit = Z.(to_int @ num / den) in
          assert (0 <= digit && digit <= 9);
          let num = Z.(num mod den) in
          let exponent_neg = 
            if Buffer.length buf = 0 && digit = 0 then
              exponent_neg + 1
            else
              (Buffer.add_char buf (Char.chr @ Char.code '0' + digit); exponent_neg)
          in
          loop buf exponent_neg num den
      in
      let num = Z.(num mod den) in
      let buf = Buffer.create max_figures |> tap (fun buf -> Buffer.add_string buf digits) in
      let exponent_neg = loop buf 0 num den + skipped_zeroes in
      let digits = Buffer.contents buf in
      let figs = min (String.length digits) max_figures in
      let digits = String.sub digits 0 figs in
      (* number = 0.decimal * exponent *)
      let exponent = (if exponent_pos > 0 then exponent_pos else -exponent_neg) in
      let[@inline] fmt_fixed () = 
        if exponent <= 0 then 
          Printf.sprintf "%s0.%s%s" sign (String.make (-exponent) '0') digits
        else if exponent < figs then
          Printf.sprintf "%s%s.%s" sign (String.sub digits 0 exponent) (String.sub digits exponent (figs-exponent))
        else
          Printf.sprintf "%s%s%s" sign digits (String.make (exponent-figs) '0')
      in
      let[@inline] fmt_scientific () = 
        if exponent-1 = 0 then
          Printf.sprintf "%s%c.%s" sign (digits.[0]) (String.sub digits 1 (figs-1))
        else
          Printf.sprintf "%s%c.%se%d" sign (digits.[0]) (String.sub digits 1 (figs-1)) (exponent-1)
      in
      match format with
      | Some `Sci -> fmt_scientific ()
      | Some `Dec -> fmt_fixed ()
      | None -> if abs exponent <= 4 then fmt_fixed () else fmt_scientific ()

  (* let pp ppf x = 
    Fmt.pf ppf "%a" Q.pp_print (to_q x) *)

  let to_int x = 
    if is_nar x then Int.min_int else
    try Q.to_int @ to_q x with Z.Overflow -> Int.min_int

  let to_float x = 
    if is_zero x then Float.zero else
    if is_nar x then Float.nan else
    Q.to_float @ to_q x



  (* Parsing *)

  (* Likewise, for testing and for parsing, we have a function to take an 
     exact rational, and round it per the standard into a posit. With [to_q] 
     and [of_q] we can design tests for the correctness of posit algorithms, 
     e.g.: verify that Posit.add x y = of_q (Zarith.Q.add (to_q x) (to_q q)). *)
  let of_q x = 
    if Q.(x = zero) then
      zero
    else if not (Q.is_real x) then
      nar
    else
      let useed = Z.shift_left Z.one (1 lsl es) in
      let maxp = Q.of_bigint @ Z.pow useed (nbits - 2) in
      let minp = Q.(one / maxp) in
      if Q.(x >= +maxp) then +max_posit else
      if Q.(x <= -maxp) then -max_posit else
      if Q.(zero < x && x <= +minp) then +min_posit else
      if Q.(zero > x && x >= -minp) then -min_posit else
      let rec normalise exp factor x = 
        (* Use increasingly high powers of 2 as divisors to make the normalisation 
           process O(logn) in the number of digits rather than O(n). *)
        if Q.(abs x >= of_int 2) then
          let factor = max factor 0 + 1 in
          normalise (exp+factor) factor Q.(div_2exp x (factor))
        else if Q.(abs x < of_int 1) then
          let factor = min factor 0 - 1 in
          normalise (exp+factor) factor Q.(mul_2exp x (Int.neg factor))
        else
          let frac = Q.mul_2exp x frac_width in
          let exact = Q.(of_int @ to_int frac = frac) in
          if exact then
            (* Return exact value *)
            let frac = Q.to_int frac in
            exp, frac, 0
          else
            (* Return rounded down, taking care to not underflow towards 
               [Int.min_int]. Since Zarith rounds towards 0, and posits round 
               down, we need to correct that, so if negative and not exact, 
               subtract one. *)
            let frac = Q.to_int frac - (Bool.to_int @ Q.(frac < zero)) in
            if frac = Int.min_int then exp+1, frac asr 1, 1 else exp, frac, 1
      in
      let exp, frac, s = normalise 0 0 x in
      encode_regular {exp; frac} ~s

  let of_int x = 
    of_q (Q.of_int x)

  let of_float x = 
    of_q (Q.of_float x)

  let of_string x = 
    if String.(equal @ lowercase_ascii @ trim x) "nar" then nar else of_q (Q.of_string x)



  (* Comparison *)

  (* Unlike floats, posit comparison is dead simple. No NaN <> NaN or 
     +0 = -0 nonsense!  *)
  let eq (x:t) y = 
    x = y

  let ne (x:t) y = 
    x <> y

  let gt (x:t) y = 
    x > y

  let ge (x:t) y = 
    x >= y

  let lt (x:t) y = 
    x < y

  let le (x:t) y = 
    x <= y



  (* Arithmetic *)

  let neg x = 
    if nbits = int_size || x <> nar then -x else x

  let abs x = 
    if nbits = int_size || x <> nar then Int.abs x else x

  let sign x = 
    if is_zero x || is_nar x then x else if x<0 then minus_one else one

  let round x = 
    todo()

  let ceil x = 
    todo()

  let floor x = 
    todo()

  let next_val x = sign_extend @ succ x
  let prev_val x = sign_extend @ pred x

  (* Arithmetic *)

  (** Add/sub: procedure is simply: 
        - Align exponents
        - Shift fractions accordingly
        - Add shifted fractions 
      There's only a couple things to pay attention to. *)
  let add_kernel x y = 
    let unequal_signs = Bits.unequal_signs x y in
    let x = decode_regular x in
    let y = decode_regular y in
    (* Put the highest exponent in x and the lowest in y *)
    let x,y = if x.exp >= y.exp then x,y else y,x in
    (* If the difference in exponents is enough that all fractional bits 
       on one of them are deleted, short-cut. This is needed, since shifts 
       greater than int_size are undefined and the general algorithm below 
       would break in these cases. *)
    if (x.exp - y.exp) >= nbits then 
      let exp = x.exp in
      (* Needed! positive stay the same but negative get 1 subtracted from them essentially *)
      let frac = x.frac + (y.frac asr nbits) in
      (* Here an underflow of 1 may apply, if they have opposite signs and one of them is ±1 *)
      let exp, frac = if (abs frac < frac_denom) then exp-1, frac lsl 1 else exp, frac in
      (* Whole y.frac would be shifted onto sticky, so s=1 *)
      encode_regular {exp; frac} ~s:1
    else
    let xfrac = x.frac in
    let yfrac, r, s = IntExt.(of_int y.frac asr (x.exp - y.exp)) in
    let frac = xfrac + yfrac in
    let exp = x.exp in
    let neg_if c x = if c >= 0 then x else IntExt.(-x) in
    (* Equal signs: may have 1 overflow *)
    if not unequal_signs then 
      (* If 1 on the msb of absolute value, then it overflowed: need to (logical) shift 1 right *)
      let frac', _, _ = neg_if xfrac (frac, r, s) in
      let shift_amount = 1 - Bits.clz frac' in
      let exp = exp + shift_amount in
      let frac, r, s = IntExt.((frac, r, s) asr shift_amount) in
      let frac = let mask = 1 lsl (int_size-1) in if xfrac > 0 then frac land (lnot mask) else frac lor mask in
      (* At least the lsb of frac is going to be shifted out, that's why we 
         don't send a round bit and just send the sticky bit as r|s. *)
      encode_regular ~s:(r lor s) {exp; frac}
    (* Different signs: may have arbitrary underflow *)
    else 
      (* Same idea, but now we shift left to put the 1 in the right place of the absolute value, i.e. second to last msb *)
      let frac', _, _ = neg_if frac (frac, r, s) in
      let shift_amount = Bits.clz frac' - 1 in
      let exp = exp - shift_amount in
      let frac = IntExt.((frac, r, s) lsl shift_amount) in
      encode_regular ~s:(r lor s) {exp; frac}
      
  let add x y = 
    if is_nar x || is_nar y then nar else 
    if is_zero x || is_zero y then x lor y else
    add_kernel x y

  let sub x y = 
    if is_nar x || is_nar y then nar else 
    if is_zero x || is_zero y then x lor y else
    (* Negation on posits is perfect *)
    add_kernel x (-y)

  let mul x y = 
    if is_nar x || is_nar y then
      nar
    else if is_zero x || is_zero y then
      zero
    else
      (** Multibyte multiplication, keeping in mind that 
          x/frac_denom * y/frac_denom = (x*y/frac_denom) / frac_denom *)
      let[@inline] mul_double x y = (* TODO: replace by native operations (multibyte multiplication) *)
        let z = Z.(of_int x * of_int y) in
        let hi = Z.(z asr frac_width) in
        let lo = Z.(z mod of_int frac_denom) in
        let s = Bool.to_int Z.Compare.(lo <> Z.zero) in
        if dbg then Printf.eprintf "%a [%a] %a %a\n" Z.output z Z.output Z.(hi asr int_size) Z.output hi Z.output lo;
        (* There can be no underflow, but we need to check for a possible overflow of 1 bit. *)
        if Z.Compare.(Z.abs hi >= Z.shift_left Z.one (frac_width+1)) then
          let hi = Z.(hi asr 1) in
          Z.to_int hi, s lor (Z.to_int hi land 1), 1
        else
          Z.to_int hi, s, 0
      in
      let x = decode_regular x in
      let y = decode_regular y in
      let frac, s, overflow = mul_double x.frac y.frac in
      let exp = x.exp + y.exp + overflow in
      encode_regular {exp; frac} ~s

  let div x y = 
    if is_zero y || is_nar x || is_nar y then
      nar
    else if is_zero x then
      zero
    else
      (** Multibyte division, keeping in mind that
          (x/frac_denom) / (y/frac_denom) = (x*frac_denom / y) / frac_denom *)
      let[@inline] div_double x y = (* TODO: replace by native operations (multibyte division) *)
        (* If x ≥ y, and since they're both ∈ [1,2[, then x/y ∈ [1,2[, so 
           there's no overflow. *)
        (* If x < y, and since they're both ∈ [1,2[, then x/y ∈ [0.5,1[, so 
           we need to shift back left to renormalise. Rather than that, we 
           just shift the numerator (x) before the division. *)
        let overflow = Bool.to_int (abs x < abs y) in
        let x' = Z.shift_left (Z.of_int x) (frac_width + overflow) in
        let y' = Z.of_int y in
        let frac = Z.to_int Z.(x' / y') in
        let inexact = Z.Compare.(Z.(x' mod y' <> zero)) in
        let s = Bool.to_int inexact in
        (* Now just correct the following: we want to round down, but zarith 
           long division rounds towards 0 *)
        let frac = frac - ((Bool.to_int inexact) land ((x lxor y) lsr (int_size-1))) in
        frac, s, -overflow
      in
      let x = decode_regular x in
      let y = decode_regular y in
      let frac, s, overflow = div_double x.frac y.frac in
      let exp = x.exp - y.exp + overflow in
      encode_regular {exp; frac} ~s



  (** The "quire" is a very powerful feature of posit arithmetic. It is 
      essentially a fixed-point accumulator / "scratch-space", on which we 
      can accumulate sums, subtractions, or more generally dot products, 
      without *any* loss of precision. There is no intermediate rounding, as 
      there would be when accumulating a dot product on a float, and there 
      is only the single rounding at the very end, when we get back the 
      final value as a posit. *)
  module Quire = struct
    (** We use a Z.t as storage for the quire. It is faster than using a 
        fixed-size ocaml array and doing bit-logic manually, but not as fast 
        as hand-rolling mutable fixed-size bigint arithmetic.

        The bit-format of the quire is a fixed-size number of max_exp*4 bits 
        (so that any posit squared can be represented in fixed-size) plus 31 
        extra bits, and a sign bit. *)
    type t = Z.t
    type q = t

    (* let quire_nbits = let x = ((nbits lsl 2) - 8) lsl es + 31 in 1 lsl (int_size - Bits.clz (x-1)) *)

    (** The quire has [half_width] bits after the decimal dot, and [half_width+31] bits before *)
    let half_width = max_exp * 2
    (** Total number of bits, including the sign bit *)
    let quire_nbits = 2 * half_width + 32 
    (** Quire represents its 2-complement number divided by [quire_denom] *)
    let quire_denom = Z.(one lsl half_width)

    (** If there would be carries into the last bit, the quire overflows *)
    let max_value = Z.pred @ Z.shift_left Z.one (quire_nbits-1)
    let min_value = Z.neg max_value

    let clamp_overflown q = 
      if Z.Compare.(q > max_value) then max_value else
      if Z.Compare.(q < min_value) then min_value else
      q

    let is_overflown q = 
      Z.Compare.(Z.abs q > max_value)

    let q_zero = Z.zero
    let q_nar = Z.shift_left Z.minus_one (quire_nbits-1)

    let is_q_zero q = Z.Compare.(q = q_zero)
    let is_q_nar q = Z.Compare.(q = q_nar)

    let to_string_dbg q = 
      if is_q_nar q then Printf.sprintf "nar\n" else
      let a = Z.extract q (0*max_exp) max_exp in
      let b = Z.extract q (1*max_exp) max_exp in
      let c = Z.extract q (2*max_exp) max_exp in
      let d = Z.extract q (3*max_exp) max_exp in
      let e = Z.extract q (4*max_exp) 31 in
      let s = Z.format "%b" in
      let sign = Bool.to_int Z.Compare.(q < q_zero) in
      let status = if is_overflown q then "INVALID" else "valid" in
      Printf.sprintf "[%s] %d [%s] %s %s %s %s = %a\n" 
        status sign (s e) (s d) (s c) (s b) (s a) Z.sprint q

    let of_p x = 
      match decode x with
      | `Zero -> q_zero
      | `Nar -> q_nar
      | `Num {exp; frac} -> 
        (* x = 2^exp × frac/(2^frac_width). We want to represent it as 
           quire/(2^half_width). So quire = 2^(half_width + exp - frac_width) × frac *)
        let shift_amount = half_width - frac_width + exp in
        (* To avoid branching since shift_amount may be positive nor negative, 
           first shift right by r (no problem, no significant digits of frac 
          in those places), then left by shift_amount+r. *)
        let r = int_size - nbits in
        let l = shift_amount + r in
        Z.shift_left (Z.of_int (frac asr r)) l

    let to_p q = 
      if is_q_nar q then nar else
      if is_q_zero q then zero else
      (* number = quire/(2^half_width). We want to represent it as 2^exp × frac, 
         with frac ∈ [1,2[. In other words, we want to shift the leading 1 into 
         the lsb of the integral part. That difference will be the exponent. *)
      let shift_amount = Z.numbits q - 1 in
      let exp = shift_amount - half_width in
      let signed_shift z i = if i>0 then Z.shift_right z i else Z.shift_left z (-i) in
      let frac = Z.to_int @ signed_shift q (shift_amount - frac_width) in
      (* If there were any nonzero bits chopped off, set the sticky bit *)
      let s = if shift_amount > frac_width then Bool.to_int @ Z.Compare.(Z.extract q 0 (shift_amount - frac_width) <> Z.zero) else 0 in
      (* Corner case *)
      let frac, exp = if frac = Int.min_int then frac asr 1, exp+1 else frac, exp in
      encode_regular {exp; frac} ~s

    let add x q = 
      if is_q_nar q || is_nar x then q_nar else
      let result = Z.(q + of_p x) in
      clamp_overflown result

    let sub x q = 
      if is_q_nar q || is_nar x then q_nar else
      let result = Z.(q - of_p x) in
      clamp_overflown result

    let add_mul x y q = 
      if is_q_nar q || is_nar x || is_nar y then q_nar else
      let result = Z.(q + ((of_p x * of_p y) asr half_width)) in
      clamp_overflown result

    let sub_mul x y q = 
      if is_q_nar q || is_nar x || is_nar y then q_nar else
      let result = Z.(q - ((of_p x * of_p y) asr half_width)) in
      clamp_overflown result

    let add_q q1 q2 = 
      if is_q_nar q1 || is_q_nar q2 then q_nar else
      let result = Z.(q1 + q2) in
      clamp_overflown result      

    let sub_q q1 q2 = 
      if is_q_nar q1 || is_q_nar q2 then q_nar else
      let result = Z.(q1 - q2) in
      clamp_overflown result      

    let neg q = 
      if is_q_nar q then q_nar else Z.neg q

    let abs q = 
      if is_q_nar q then q_nar else Z.abs q

    let of_q x = 
      if not @ Q.is_real x then q_nar else
      let limit_hi = Q.make max_value quire_denom in
      let limit_lo = Q.make Z.one quire_denom in
      if Q.(            x > +limit_hi) then Z.(+max_value) else
      if Q.(x > zero && x < +limit_lo) then Z.one else
      if Q.(x < zero && x > -limit_lo) then Z.minus_one else
      if Q.(            x < -limit_hi) then Z.(-min_value) else
      Q.(to_bigint (x lsl half_width))

    let to_q x = 
      if is_q_nar x then Q.undef else
      Q.make x quire_denom

    let empty = q_zero
    let is_real x = Z.Compare.(x <> q_nar)
  end

  (* Convenience functions, all internally use a quire to be exact! *)
  let sum x = 
    Quire.to_p @ Seq.fold_left (fun q x -> Quire.add x q) Quire.empty x

  let sum_list x = 
    Quire.to_p @ List.fold_left (fun q x -> Quire.add x q) Quire.empty x

  let sum_array x = 
    Quire.to_p @ Array.fold_left (fun q x -> Quire.add x q) Quire.empty x

  let dot_prod x y = 
    Quire.to_p @ Seq.fold_left2 (fun q x y -> Quire.add_mul x y q) Quire.empty x y

  let dot_prod_list x y = 
    Quire.to_p @ List.fold_left2 (fun q x y -> Quire.add_mul x y q) Quire.empty x y

  let dot_prod_array x y = 
    Quire.to_p @ Seq.fold_left2 (fun q x y -> Quire.add_mul x y q) Quire.empty (Array.to_seq x) (Array.to_seq y)



  (* let (~$) = of_string *)

  module O = struct
    let (~+) = Fun.id
    let (~-) = neg
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let (/) = div
    let (=) = eq
    let (<>) = ne
    let (<) = lt
    let (<=) = le
    let (>) = gt
    let (>=) = ge
  end

  include O
end

(** Standard-compliant (except 64-bits is 63-bits) *)
module P8 = Make(struct let nbits = 8 let es = 2 end)
module P16 = Make(struct let nbits = 16 let es = 2 end)
module P32 = Make(struct let nbits = 32 let es = 2 end)
module P63 = Make(struct let nbits = 63 let es = 2 end)
