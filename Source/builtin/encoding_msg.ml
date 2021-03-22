(** Encoding Strings *)

(*#mod_use "builtin.ml"
#mod_use "basic_arithmetics.ml"
#mod_use "power.ml"*)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let rec aux a t i =
    match i with
      i when i > (-1) -> aux (a + Char.code(str.[t]) * (power 2 (bits * i))) (t + 1) (i - 1)
    |_ -> a
  in aux 0 0 ((String.length str) - 1)

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec aux msg =
    match msg with
      0 -> ""
    |_ -> aux (msg / (power 2 bits)) ^ Char.escaped(Char.chr(modulo msg (power 2 bits)))
  in aux msg
