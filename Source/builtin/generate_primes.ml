(** Generating primes *)

(*#mod_use "builtin.ml"
#mod_use "basic_arithmetics.ml"*)

open Builtin
open Basic_arithmetics

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n =
  let rec aux n l =
    match n with
      n when n < 2 -> []
    |n when n = 2 -> 2 :: l
    |n when modulo n 2 = 0 -> aux (n-1) l
    |_ -> aux (n-1) (n :: l)
  in aux n []

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
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


(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let openfile = open_out file
  in
  let rec aux l =
    match l with
      [] -> close_out openfile
    |e::l -> Printf.fprintf openfile "%d\n" e; aux l
  in
  aux li

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = []

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = let ic = open_in file in
  let try_read() =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop () = match try_read () with
      Some s -> (int_of_string s)::(loop ())
    |None -> close_in ic; []
  in loop ()

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  if limit < 2 then []
  else let rec aux i =
         match i with
         _ when i > limit -> []
         |_ -> if (isprime i) && (isprime ((i*2) + 1))
           then (i, ((i*2)+1))::aux (i+1)
           else aux (i +1)
    in aux 2

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  if limit < 2 then []
  else
    let rec tmp l i =
      match i with
        _ when i < 4 -> l
      |_ -> if isprime i && isprime (i - 2)
        then tmp (((i - 2), i)::l) (i - 1)
        else tmp l (i - 1)
    in tmp [] limit
