(** Ciphers
    Built-in integer based ciphers.
*)

(*#mod_use "builtin.ml"
#mod_use "basic_arithmetics.ml"
#mod_use "power.ml"*)


open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let encrypt_cesar k m b =
  let rec aux k m b =
    match m with
      [] -> []
    |e::m -> let sum = (e+k)
      in if sum > b then (sum-b)::aux k m b
      else if sum < 0 then (sum+b)::aux k m b
      else sum::aux k m b
  in aux k m b

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b =
  let rec aux k m b =
    match m with
      [] -> []
    |e::m -> let sum = (e-k)
      in if sum > b then (sum-b)::aux k m b
      else if sum < 0 then (sum+b)::aux k m b
      else sum::aux k m b
  in aux k m b

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p * q
  in let t = (p - 1) * (q - 1)
  in let x = 65537
  in let (d,_,_) = bezout x t
  in ((n,x),(n,d))

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)

let rec public_data_g p =
  let g = Random.int (p -1)
  in if (prime_mod_power g 2 p) = 1 then (g, p)
  else public_data_g p

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

let generate_keys_g (g, p) =
  let x = (Random.int (p - 1))
  in let h = prime_mod_power g x p
     in (h, x)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)

let encrypt_g msg (g, p) kA =
  let k = Random.int 100000
  in let c1 = prime_mod_power g k p
     in let c2 = msg * (power kA k)
        in (c1, c2)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

let mod_inv (e,m) =
  let e1 = modulo e m
  in let rec aux e1 m x =
       if x = m then x
       else
       if modulo (e1 * x) m = 1 then x
       else aux e1 m (x + 1)
  in aux e1 m 1


let decrypt_g (msgA, msgB) a (g, p) =
  (power msgA (a-1))*msgB
