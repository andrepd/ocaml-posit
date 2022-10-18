module type Arith = sig 
  type t
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (~$) : float -> t
  val to_string : t -> string
end

module Float = struct
  type t = float
  let (=) = Float.equal
  let (<>) x y = not (Float.equal x y)
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *.)
  let (/) = (/.)
  let (~$) = Fun.id
  let to_string = Float.to_string
end

module Posit = struct
  include Posit.P63
  let (~$) = of_float
  let to_string = to_string ?format:None ?precision:None
end

(* Example 2x2 linear system:

     (a b) × (x) = (u)
     (c d)   (y)   (v)

   We pick pathologically bad values for a,b,c,d, such that the matrix is 
   *almost* singular, but not quite. 

   Source: The End of Error, ISBN 9781482239867 *)

let a = 25510582
let b = 52746197
let c = 80143857
let d = 165707065

let u = 79981812
let v = 251270273

let () = 
  Printf.printf "Linear system:\n";
  Printf.printf "(%9d %9d) × (x) = (%9d)\n" a b u;
  Printf.printf "(%9d %9d)   (y)   (%9d)\n" c d v;
  print_newline ()

(* The followig are represented *exactly* as 64-bit floats, so there is no 
   problem with loss of precision at this stage. *)

let () = 
  Printf.printf "We divide all coefficients by 2^26 (still exactly representable in 64-bit floats).\n\n"

let a = float_of_int a /. 2.**26.
let b = float_of_int b /. 2.**26.
let c = float_of_int c /. 2.**26.
let d = float_of_int d /. 2.**26.
let u = float_of_int u /. 2.**26.
let v = float_of_int v /. 2.**26.

(* The system has a simple solution: -1, 2. Both posits and floats can verify it. *)

let check_solution (type a) (module M : Arith with type t = a) x y = 
  let open M in
  ~$a * ~$x + ~$b * ~$y - ~$u,
  ~$c * ~$x + ~$d * ~$y - ~$v

let () = 
  let x, y = check_solution (module Float) (-1.) (2.) in
  Printf.printf "Floats, replace (x y) = (-1 2)\n";
  Printf.printf "(a b) × (-1) - (u) = (%g)\n" x;
  Printf.printf "(c d)   ( 2)   (v)   (%g)\n" y;
  print_newline ();
  let x, y = check_solution (module Posit) (-1.) (2.) in
  Printf.printf "Posits, replace (x y) = (-1 2)\n";
  Printf.printf "(a b) × (-1) - (u) = (%s)\n" (Posit.to_string x);
  Printf.printf "(c d)   ( 2)   (v)   (%s)\n" (Posit.to_string y);
  print_newline ()

(* But can both posits and floats *calculate* it? *)

let calculate_solution (module M : Arith) = 
  let open M in
  let det = ~$a * ~$d - ~$b * ~$c in
  let x = (~$u * ~$d - ~$b * ~$v) / det in
  let y = (~$a * ~$v - ~$u * ~$c) / det in
  to_string x, to_string y

let () = 
  let x, y = calculate_solution (module Float) in
  Printf.printf "Float solution: (x y) = (%s %s)\n" x y;
  let x, y = calculate_solution (module Posit) in
  Printf.printf "Posit solution: (x y) = (%s %s)\n" x y;

(* There is a catastrophic loss of precision! *)
