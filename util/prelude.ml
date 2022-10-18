let todo() = assert false
(* let[@inline] (%%) f g = fun x -> f (g x) *)
let (@) = (@@)
let (@@) = List.append

module Poly = struct
  let (= ) = (= )
  let (<>) = (<>)
  let (< ) = (< )
  let (<=) = (<=)
  let (> ) = (> )
  let (>=) = (>=)
  let min = min
  let max = max
  let compare = compare
  let min_max x y = if x<=y then x,y else y,x
end

let (= ) (x:int) y = x =  y
let (<>) (x:int) y = x <> y
let (< ) (x:int) y = x <  y
let (<=) (x:int) y = x <= y
let (> ) (x:int) y = x >  y
let (>=) (x:int) y = x >= y
let min (x:int) y = min x y
let max (x:int) y = max x y
let compare (x:int) y = compare x y
let min_max (x:int) y = if x<=y then x,y else y,x

let bool_of_int : int -> bool = Obj.magic

let tap f x = f x; x
