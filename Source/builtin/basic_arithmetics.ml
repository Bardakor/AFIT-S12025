(** Basic arithmetics with built-in integers *)

(*#mod_use "builtin.ml"*)

open Builtin

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let rec gcd a b =
  if b = 0 then a else sign(gcd b (a mod b))*(gcd b (a mod b))

(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  if b = 0 then ( a , 1 , 0 )
  else let rec aux (u,v,r,u2,v2,r2) =
         if r2 = 0 then (u,v,r)
         else aux (u2,v2,r2, u-r/r2*u2,v-r/r2*v2,r-r/r2*r2)
       in
aux (1,0,a,0,1,b)

