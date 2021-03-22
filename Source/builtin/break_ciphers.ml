(** Factoring Built-In Int Primes *)

(*#mod_use "builtin.ml"
#mod_use "basic_arithmetics.ml"*)


open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)

let init_eratosthenes n =
  let rec aux n l =
    match n with
      n when n < 2 -> []
    |n when n = 2 -> 2 :: l
    |n when modulo n 2 = 0 -> aux (n-1) l
    |_ -> aux (n-1) (n :: l)
  in aux n []

let eratosthenes n =
  let rec aux1 el l =
    match l with
      [] -> []
    |e::l ->
      if modulo e el = 0 then aux1 el l
      else e::(aux1 el l)
  in
  let rec aux2 l=
    match l with
      [] -> []
    |el::l -> el::(aux2(aux1 el l))
  in
  aux2 (init_eratosthenes n)

let rec find_q p n l =
  match l with
    [] -> false
  |e::l when (e * p) = n -> true
  |e::l -> find_q p n l

let break key =
  match key with
    (n,_) ->
    let rec find_p n l =
      match l with
        [] -> (0,0)
      |e::l when find_q e n l -> (e, quot n e)
      |e::l -> find_p n l
    in find_p n (eratosthenes 20000)
