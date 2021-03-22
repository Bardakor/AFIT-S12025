(** Testing for primality *)


(*#mod_use "builtin.ml"
#mod_use "basic_arithmetics.ml"
#mod_use "power.ml"*)


open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = let rec tmp n i =
    match n with
        _ when i * i > n -> true
      |_ when modulo n i == 0 -> false
      |_ -> tmp n (i + 1) in tmp n 2

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let rec is_pseudo_prime p test_seq =
  if p = 2 then true
        else
          match test_seq with
            [] -> true
          |(e::test_seq) when gcd p e == 1 -> is_pseudo_prime p test_seq
          |_ -> false
