(** A module of type [Posit] encapsulates the type of posit values of a given 
    [nbits] and [es], and their operations, as well as the quire associated 
    with that posit. *)
module type Posit = sig
  (** The number of bits of this posit *)
  val nbits : int
  (** The number of exponent bits of this posit *)
  val es : int

  (** A posit *)
  type t

  (** 0, 1, -1, and NaR (not-a-real), respectively. *)
  val zero : t
  val one : t
  val minus_one : t
  val nar : t

  (** As [eq x zero]. *)
  val is_zero : t -> bool

  (** As [eq x nar]. *)
  val is_nar : t -> bool

  (** As [ne x nar]. *)
  val is_real : t -> bool

  (** The largest representable posit (the smallest is [-max_posit]). *)
  val max_posit : t

  (** The smallest representable positive posit (the largest negative is 
      [-min_posit]). *)
  val min_posit : t

  (** Between -[pure_int_range] and [pure_int_range] all integers are exactly 
      representable by a posit. *)
  (* val max_pure_int_range : int *)

  (** {2 Create/parse} *)

  (** Create a posit directly from a binary value. Only the lowest [nbits] 
      bits are used, the rest are discarded. *)
  val of_bits : int -> t

  (** Create a posit from an [int] value, rounding if necessary. *)
  val of_int : int -> t

  (** Create a posit from a [float] value, rounding if necessary. *)
  val of_float : float -> t

  (** Create a posit from a [string] value, rounding if necessary. *)
  val of_string : string -> t

  (** Convert from a different posit type. If both posits have the same [es], 
      a conversion from a lower-sized posit is exact, otherwise may round. *)
  (* val of_posit : (module P : Posit) -> P.t -> t *)

  (** {2 Convert/output} *)

  (** Convert a posit to an [int] value, (may round). If the posit represents 
      a number outside [int] range, [Int.min_value] is returned, as per the 
      posit standard. *)
  val to_int : t -> int

  (** Convert a posit to a [float] value (may round). *)
  val to_float : t -> float
  
  (** Print the posit as a number, with enough significant digits so that it 
      is guaranteed that [of_string (to_string x) = x], but not necessarily 
      more digits than that. If [precision] is specified, it is printed with 
      (at most) that many significant digits instead. If [format] is specified 
      as [`Dec], it is printed in decimal/scientific notation isntead (the 
      default behaviour is to print numbers in \[1e-4,1e4\] as decimals, and 
      numbers outside that interval in scientific notation. 

      Note: any extra digits are *truncated*, not rounded. *)
  val to_string : ?format:[`Dec | `Sci] -> ?precision:int -> t -> string

  (** Convert to a different posit. If both posits have the same [es], a 
      conversion to a higher-sized posit is exact, otherwise may round. *)
  (* val to_posit : (module P : Posit) -> t -> P.t *)

  (** {2 Comparisons} *)

  (** Equal, not equal, less than, less or equal, greater than, greater or 
      equal, respectively. Always compares as less than any real number. *)
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool

  val (=)  : t -> t -> bool
  val (<>) : t -> t -> bool
  val (<)  : t -> t -> bool
  val (<=) : t -> t -> bool
  val (>)  : t -> t -> bool
  val (>=) : t -> t -> bool

  (** {2 Basic unary operations} *)

  (** Identity *)
  val (~+) : t -> t

  (** Negate the posit (exact) *)
  val neg : t -> t
  val (~-) : t -> t

  (** Absolute value (exact) *)
  val abs : t -> t

  (** If [x > zero], [sign x] = [one]. If [x < zero], [sign x] = [minus_one]. 
      If [x = zero], [sign x] = [zero] (exact). *)
  val sign : t -> t

  (** Round to nearest integer (ties round to nearest even). *)
  val round : t -> t

  (** Round to greatest integer not greater than it. *)
  val floor : t -> t

  (** Round to smallest integer not smaller than it. *)
  val ceil : t -> t

  (** Lexicographically next posit, wrapping if necessary (exact). *)
  val next_val : t -> t
  val prev_val : t -> t

  (** {2 Elementary arithmetic} *)

  (** Add two posits (may round, commutative). *)
  val add : t -> t -> t
  val (+) : t -> t -> t

  (** Subtract second posit from first (may round). *)
  val sub : t -> t -> t
  val (-) : t -> t -> t

  (** Multiply two posits (may round, commutative). *)
  val mul : t -> t -> t
  val ( * ) : t -> t -> t

  (** Divide first posit by second (may round). *)
  val div : t -> t -> t
  val (/) : t -> t -> t

  (** {2 Quire} *)

  module Quire : sig
    (** The type of a quire associated with this posit *)
    type q

    (** Convert a posit value into a quire value (exact). *)
    val of_p : t -> q

    (** Convert a quire value into a posit value (may round). *)
    val to_p : q -> t

    (** Add a posit to the quire (exact, up to 2^30 terms). *)
    val add : t -> q -> q

    (** Subtract a posit from the quire (exact, up to 2^30 terms). *)
    val sub : t -> q -> q

    (** Add the product of two posits to the quire (exact, up to 2^30 
        terms). *)
    val add_mul : t -> t -> q -> q

    (** Subtract the product of two posits from the quire (exact, up to 
        2^30 terms). *)
    val sub_mul : t -> t -> q -> q

    (** Negate the quire (exact). *)
    val neg : q -> q

    (** Absolute value (exact). *)
    val abs : q -> q

    (** Checks if the quire value is not NaR. If accumulating only real terms, 
        this is guaranteed not to happen until at least 2^30 terms have been 
        accumulated. *)
    val is_real : q -> bool
  end

  (** {2 Quire-backed sums and dot products} *)

  (** Sum (may round, but only once, no matter the number of terms). *)
  val sum : t Seq.t -> t

  (** Dot product (may round, but only once, no matter the number of terms). 

      @raises [Invalid_argument] if the two vectors have different lengths. *)
  val dot_prod : t Seq.t -> t Seq.t -> t

  (** Convenience functions for lists and arrays *)
  val sum_list : t list -> t
  val dot_prod_list : t list -> t list -> t

  val sum_array : t array -> t
  val dot_prod_array : t array -> t array -> t

  (** Convenient module for local [open]s. *)
  module O : sig
    val (~+) : t -> t
    val (~-) : t -> t
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : t -> t -> t
    val (/) : t -> t -> t
    val (=)  : t -> t -> bool
    val (<>) : t -> t -> bool
    val (<)  : t -> t -> bool
    val (<=) : t -> t -> bool
    val (>)  : t -> t -> bool
    val (>=) : t -> t -> bool
  end
end

(** The main interface to create a Posit type of a custom size ([nbits]) and 
    exponent size ([es]). 
    Constraints: 2 ≤ es ≤ nbits ≤ [Sys.int_size]. *)
module Make (Params : sig val nbits : int val es : int end) : Posit

(** Posit with [nbits]=8, [es]=2 (from posit standard). *)
module P8 : Posit

(** Posit with [nbits]=16, [es]=2 (from posit standard). *)
module P16 : Posit

(** Posit with [nbits]=32, [es]=2 (from posit standard). *)
module P32 : Posit

(** Posit with [nbits]=63, [es]=2 (posit standard has [nbits]=64, but that 
    would not fit in an OCaml immediate in 64-bit platforms). *)
module P63 : Posit

(** For the hardcoded types, some extra convenience functions are provided *)
(* module type DefinedPosit = sig
  include Posit

  val of_p8  : P8.t  -> t
  val of_p16 : P16.t -> t
  val of_p32 : P32.t -> t
  val of_p63 : P63.t -> t

  val to_p8  : t -> P8.t
  val to_p16 : t -> P16.t
  val to_p32 : t -> P32.t
  val to_p63 : t -> P63.t
end *)
