include Impl

module type Posit = sig
  val nbits : int
  val es : int
  type t
  val zero : t
  val one : t
  val minus_one : t
  val nar : t
  val is_zero : t -> bool
  val is_nar : t -> bool
  val is_real : t -> bool
  val max_posit : t
  val min_posit : t
  val of_bits : int -> t
  val of_int : int -> t
  val of_float : float -> t
  val of_string : string -> t
  (* val of_posit : (module P : Posit) -> t -> P.t *)
  val to_int : t -> int
  val to_float : t -> float
  val to_string : ?format:[`Dec | `Sci] -> ?precision:int -> t -> string
  (* val to_posit : (module P : Posit) -> t -> P.t *)
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
  val (~+) : t -> t
  val neg : t -> t
  val (~-) : t -> t
  val abs : t -> t
  val sign : t -> t
  val round : t -> t
  val floor : t -> t
  val ceil : t -> t
  val next_val : t -> t
  val prev_val : t -> t
  val add : t -> t -> t
  val (+) : t -> t -> t
  val sub : t -> t -> t
  val (-) : t -> t -> t
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
  val div : t -> t -> t
  val (/) : t -> t -> t
  module Quire : sig
    type q
    val of_p : t -> q
    val to_p : q -> t
    val add : t -> q -> q
    val sub : t -> q -> q
    val add_mul : t -> t -> q -> q
    val sub_mul : t -> t -> q -> q
    val neg : q -> q
    val abs : q -> q
    val is_real : q -> bool
  end
  val sum : t Seq.t -> t
  val dot_prod : t Seq.t -> t Seq.t -> t
  val sum_list : t list -> t
  val dot_prod_list : t list -> t list -> t
  val sum_array : t array -> t
  val dot_prod_array : t array -> t array -> t
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
