ocaml-posit
===========

An ocaml library for **posits**, aka Type III unums. (WIP)

*Disclaimer: this library is a work in progress, and is not fully compliant with the Posit standard yet (see Features).*

## Introduction

Posits are a floating-point format which is a drop-in replacement for IEEE floats. 
They offer more precision than floats, at a smaller memory cost, are customisable for your specific application, and may allow for smaller, simpler, faster, and more energy-efficient hardware implementations. 

They are pretty cool, you should read about them [here](https://posithub.org/docs/Posits4.pdf) or [here](https://posithub.org/docs/posit_standard-2.pdf) or [here](https://groups.google.com/forum/?pli=1#!forum/unum-computing). 

Here are some neat features to whet your appetite:

- Higher accuracy and higher dynamic range for the same number of bits, compared to IEEE floats. 
- *Tapered accuracy*: posits elegantly allocate more bits to the mantissa for values close to ±1, gradually decreasing the precision as the absolute value of the exponent increases.\*
- Lower memory usage for the same accuracy (e.g. in some applications such as ML, 8-bit posits can outperform 16-bit floats).
- Smaller decimal error for virtually all unary and binary arithmetic operations in the posit standard. 
- Sums and dot products (up to $2^{30}$ terms) can be efficiently calculated with NO intermediate rounding. 
- Flexibility: choose any bit width ≥ 2 and any exponent width ≤ bit width, tailored to the parameters of your application: accuracy, dynamic range, memory constraints, etc. 
- Simple and deterministic rounding, with bounded errors, and no infinite under/overflow. 
- No redundant representations (only one "NaN" and one "0"), no subnormals. 
- Blazing fast sigmoid (2 integer instructions), for ML people. 
- Simpler (and smaller and more power-efficient) hardware implementations. 

\*For example: 64-bit posits beat 64-bit floats for precision in the range $10^{-17} \le |x| \le 10^{+17}$. 
By contrast, floats waste the same amount of precision for numbers in those “natural” orders of magnitude (close to 1, where most real-world values lie), as they do on the seldom-used exponents up to $10^{310}$ (there are only ~$10^{80}$ atoms in the universe!).

## Features

This is the current state of implementation. 

- [x] Arbitrary-sized posits
- [x] Basic arithmetic (+, -, ×, ÷)
- [ ] Elementary functions (sqrt, exp, log, etc)
- [ ] Trigonometric functions (sin, cos, sinh, arctan2, hypot, etc)
- [ ] Valids
- [x] Quires

## Code style

This implementation is being carefully tuned for performance, but do keep in mind that this is a software implementation of posits written in a high-level language where integers are tagged: it's just never going to be competitive in terms of FLOPS with hand-rolled C/asm, even among software implementations, let alone hardware-level ones.

More importantly, priority has been given to clarity and readability; 
in fact, I've tried to make the code reasonably “educational”, 
so I've documented the implementation of standard-compliant functions carefully, such that any non-obvious steps are explained 
(including things like encoding and decoding, arithmetic operations, and standard-compliant rounding, which were non-obvious, to me at least). 
The aim is that a person wishing to write their own implementation of posit arithmetic can read this code to understand how to implement the operations in the standard. 

I learned a lot while writing this library, hopefully you might learn something from reading my code too! 

## Installation

```
git clone <this repo>
cd <this repo>
opam pin posit .
opam install posit
```

This library depends on Zarith (arbitrary-precision rational arithmetic, used for parsing and output, as well as testing) and ocaml_intrinsics. 
If you would like to run the tests, this also requires alcotest and qcheck. 

## Usage

The library includes the four basic posit types from the standard (with the 64-bit posit being replaced by a 63-bit one, so that it fits in an unboxed ocaml immediate), as `P8`, `P16`, `P32`, and `P63`. 
It also defines a functor `Make` that gives you a posit type of a custom width (`nbits`, up to `Sys.int_size`) and exponent width (`es`, up to `nbits`). 

You can do math via operators and functions, and convert to and from strings, ints, or floats. 

```ocaml
open Posit
let a = P32.of_int 4 in
let b = P32.of_float 1.5 in
let c = P32.of_string "3.5" in
assert P32.(sqrt a + b = c)
```

You can also convert to and from other posits. 

```ocaml
open Posit
let x = P32.of_string "3.14159" in
let y = P32.to_posit (module P8) x in
assert (String.equal (P8.to_string x) "3.25")
```

The quire functionality can be used manually, or through the `sum` and `dot_prod` functions. 

```ocaml
open Posit
let make_vec len = 
  Seq.init len (fun _ -> Random.int 100_000) 
  |> Seq.map P63.of_int 
in
let v1 = make_vec 10_000 in
let v2 = make_vec 10_000 in
print_endline (P63.to_string (P63.dot_prod v1 v2))
```

Consult the documentation for details. 
