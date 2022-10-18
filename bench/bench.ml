open! Util.Prelude

module P63 = Posit__Impl.P63

let () = 
  Random.self_init()

let rand_posit _ = 
  P63.of_bits (Random.bits ())

let rand_float _ = 
  Random.float Float.max_float

let posit f _ = 
  let x = rand_posit () in
  let y = rand_posit () in
  fun () -> f x y

let zarith f _ = 
  let x = rand_posit () in
  let y = rand_posit () in
  fun () -> P63.of_q @ f (P63.to_q x) (P63.to_q y)

let float f _ = 
  let x = rand_float () in
  let y = rand_float () in
  fun () -> f x y

let () =
  let open Core_bench in
  let run_config = Bench.Run_config.create ~quota:(Bench.Quota.Span (Core.Time.Span.create ~sec:4 ())) () in 
  Bench.bench ~run_config [
    Bench.Test.create_with_initialization ~name:"add posit" (posit P63.add);
    Bench.Test.create_with_initialization ~name:"add zarith" (zarith Q.add);
    Bench.Test.create_with_initialization ~name:"add float" (float Float.add);
    Bench.Test.create_with_initialization ~name:"sub posit" (posit P63.sub);
    Bench.Test.create_with_initialization ~name:"sub zarith" (zarith Q.sub);
    Bench.Test.create_with_initialization ~name:"sub float" (float Float.sub);
    Bench.Test.create_with_initialization ~name:"mul posit" (posit P63.mul);
    Bench.Test.create_with_initialization ~name:"mul zarith" (zarith Q.mul);
    Bench.Test.create_with_initialization ~name:"mul float" (float Float.mul);
    Bench.Test.create_with_initialization ~name:"div posit" (posit P63.div);
    Bench.Test.create_with_initialization ~name:"div zarith" (zarith Q.div);
    Bench.Test.create_with_initialization ~name:"div float" (float Float.div);
  ]
