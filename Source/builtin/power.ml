(** Power function implementations for built-in integers *)

(*#mod_use "builtin.ml"
#mod_use "basic_arithmetics.ml"*)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  if x = 0 then 0
  else
    let rec tmp x n result =
      if n < 0 then tmp (1 / x) n result
      else
        match n with
          0 -> result
         |_ -> (tmp x (n-1) (result * x))
    in tmp x n 1

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let rec power x n =
  if n = 0 then 1
  else
    let rec tmp x n =
      match n with
          _ when modulo n 2 = 0 -> pow x (n / 2) * pow x (n / 2)
        |_ -> pow x (n / 2) * pow x (n / 2) * x
    in tmp x n

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let rec mod_power x n m =
  if m = 1
  then 0
  else
    let c = 1 in
    let rec aux c x n m = match n with
        0 -> c
      |_ -> aux (modulo (c*x) m) x (n-1) m
    in
    aux c x n m

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  match x with
      0 -> 0
     |_ -> mod_power x (modulo n (p-1)) p
