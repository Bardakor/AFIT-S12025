(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

let quot a b =
  if b <= 0 then failwith "b must be positive"
  else
  if a >= 0 || a mod b = 0 then a/b else (a/b) -1

let modulo a b = a - (quot a b*b)

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


let rec power x n =
  if n = 0 then 1
  else
    let rec tmp x n =
      match n with
          _ when modulo n 2 = 0 -> pow x (n / 2) * pow x (n / 2)
        |_ -> pow x (n / 2) * pow x (n / 2) * x
    in tmp x n


(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let rec tmp x l =
    match x with
      1 -> 1::l
     |_ -> (modulo x 2)::(tmp (x/2) l)
  in tmp x []

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
  let l = List.length bA in
  let rec tmp bA n i=
    match bA with
      [] -> n
     |e::l when e==1 -> tmp bA ((power 2 i)+n) (i-1)
     |_::l -> tmp bA n (i-1)
  in tmp bA 0 (l-1)

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec tmp bA =
    match bA with
      [] -> ()
     |e::l -> tmp bA; print_int(e) in
  tmp bA

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec reverse_list l =
  let rec tmp l1 l2 =
    match l1 with
      [] -> l2
     |e::l1 -> tmp l1 (e::l2)
             in tmp l []

let rec compare_n nA nB =
  let rec tmp nA nB =
    match (nA,nB) with
      ([],[]) -> 0
     |(_::_,[]) -> 1
     |([],_::_) -> -1
     |(e1::nA,e2::nB) -> if e1 > e2 then 1
                         else if nA < nB then -1
                         else 0
  in tmp nA nB

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB =
  let rec tmp nA nB =
    match (compare nA nB) with
      1 -> true
     |_ -> false
  in tmp (reverse_list nA) (reverse_list nB)

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
  match (compare_n nA nB) with
    (-1) -> true
  |_ -> false

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
  match (compare_n nA nB) with
    1 -> false
  |_ -> true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
  match (compare_n nA nB) with
    (-1) -> true
  |_ -> false

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  match (bA, bB) with
      ([],[]) -> 0
    |(_::_,[]) -> 1
    |([],_::_) -> (-1)
   |(a::bA,b::bB) ->
     match (a,b) with
       (1,0) -> (-1)
      |(0,1) -> 1
      |(1,1) -> (-1) * (compare_n bA bB)
      |_ -> compare_n bA bB

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
  match (compare_n bA bB) with
    1 -> true
  |_ -> false

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  match (compare_n bA bB) with
    (-1) -> true
  |_ -> false

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
  match (compare_b bA bB) with
    (-1) -> true
  |_ -> false

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB =
  match (compare_n bA bB) with
    1 -> true
  |_ -> false

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
    [] -> 1
   |e::bA -> match e with
               1 -> (-1)
             |_ -> 1

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with
    [] -> []
   |e::bA -> match e with
               1 -> 0::bA
              |_ -> 0::bA

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a =
  match a with
    a when a >= 2 -> 1
  |_ -> 0

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a =
  match a with
    a when a = 3 -> 1
   |a when a = 1 -> 1
   |_ -> 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)

let add_n nA nB =
  let e2 = 0 in
  let rec tmp nA nB r =
    match (nA,nB) with
        ([],[]) -> if r = 1 then r::[] else []
      |(e1::nA,e2::nB) ->
        (match (e1+e2+r) with
            0 -> 0::(tmp nA nB 0)
          |1 -> 1::(tmp nA nB 0)
          |2 -> 0::(tmp nA nB 1)
          |_ -> 1::(tmp nA nB 1))
      |(e1::nA,[]) ->
        (match (e1+e2+r) with
            0 -> 0::(tmp nA nB 0)
          |1 -> 1::(tmp nA nB 0)
          |_ -> 0::(tmp nA nB 1))
      |([],e1::nB) ->
          (match (e1+e2+r) with
              0 -> 0::(tmp nA nB 0)
               |1 -> 1::(tmp nA nB 0)
               |_ -> 0::(tmp nA nB 1))
  in tmp nA nB 0

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let e2 = 0 in
    let rec tmp nA nB r =
        match (nA,nB) with
          ([],[]) -> []
         |(e1::nA,e2::nB) ->
           (match (e1+e2+r) with
              0 -> 0::tmp nA nB 0
             |1 -> 1::tmp nA nB 0
             |2 -> 1::tmp nA nB 1
             |_ -> 1::tmp nA nB 1)
         |(e1::nA,[]) ->
           (match (e1+e2+r) with
              0 -> 0::(tmp nA nB 0)
             |1 -> 1::(tmp nA nB 0)
             |_ -> 0::(tmp nA nB 1))
         |([],e1::nB) ->
           (match (e1+e2+r) with
              0 -> 0::(tmp nA nB 0)
             |1 -> 1::(tmp nA nB 0)
             |_ -> 0::(tmp nA nB 1))
    in tmp nA nB 0


(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let inv_abs b =
  match b with
      [] -> []
    |_::b -> 1::b

let add_b bA bB =
  let tmp bA bB =
    match (bA, bB) with
        ([],[]) -> []
      |(_,[]) -> bA
      |([], _) -> bB
      |(_::bA,_::bB) ->
        let tmp2 bA bB =
          match (sign_b bA,sign_b bB) with
              (1,1) -> add_n bA bB
            |(1,-1) when (>>=) bA bB -> inv_abs (diff_n (abs_b bA) bB)
            |(1,-1) -> inv_abs (diff_n bA (abs_b bB))
            |(-1,1) when (>>=) bA bB -> inv_abs (diff_n (abs_b bA) bB)
            |(-1,1) -> diff_n (abs_b bA) bB
            |(-1,-1) -> inv_abs (add_n (abs_b bA) (abs_b bB))
            |_ -> []
        in tmp2 bA bB
  in tmp bA bB

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
  let tmp bA bB =
    match (sign_b bA,sign_b bB) with
        (1,1) when (>>) bA bB -> diff_n bA bB
      |(1,1) -> inv_abs (diff_n bB bA)
      |((-1),(-1)) when (>>) bA bB -> inv_abs (add_n (abs_b bA) (abs_b bB))
      |((-1),(-1)) -> diff_n (abs_b bB) (abs_b bA)
      |((-1),1) -> add_b bA bB
      |(1,(-1)) -> add_n bA (abs_b bB)
      |_ -> []
  in tmp bA bB

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d =
  match d with
      0 -> bA
    |_ -> 0::(shift bA (d-1))

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  let tmp1 bA bB =
    match (bA,bB) with
        ([],[])|(_,[])|([],_) -> []
      |(e1::bA,e2::bB) ->
        let rec tmp2 bA bB e1 e2 c result =
          match (bA,bB) with
              ([],[]) ->
                if e1+e2=2 || e1+e2=0 then shift result 1
                else inv_abs(shift result 1)
            |([],e::bB) ->
              if e1+e2=2 || e1+e2=0 then shift result 1
              else inv_abs(shift result 1)
            |(e::bA,[]) ->
              if e1+e2=2 || e1+e2=0 then shift result 1
              else inv_abs(shift result 1)
            |(_,e::bB) when e=1 -> tmp2 bA bB e1 e2 (c+1) (add_n result (shift bA c))
            |(_,e::bB) when e=0 -> tmp2 bA bB e1 e2 (c+1) result
            |(_,e::bB) -> tmp2 bA bB e1 e2 (c+1) result
        in tmp2 bA bB e1 e2 0 []
  in tmp1 bA bB

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let remove_n n =
  let rec tmp n =
    match n with
        [] -> []
      |e::n when e = 0 -> tmp n
      |e::n -> e::n
  in tmp n

let quot_b bA bB =
  match (bA,bB) with
      ([],[])|(_,[]) -> failwith "Can't divide by 0"
    |([],_) -> []
    |(e1::bA,e2::bB) ->
      let rec tmp bA bB q result =
        match (bA,bB) with
            ([],[]) -> if e1=e2 then shift q 1 else inv_abs (shift (reverse_list q) 1)
          |([],_) when ((>=!) (reverse_list result) (reverse_list bB)) ->
            if e1=e2 then shift (add_n (shift q 1) [1]) 1
            else  inv_abs (shift (reverse_list (add_n (shift q 1) [1])) 1)
          |([],_) ->
            if e1=e2 then shift q 1
            else inv_abs (shift q 1)
          |_ when ((>=!) (reverse_list result) (reverse_list bB)) -> tmp bA bB (add_n q [1]) (reverse_list (remove_n (reverse_list (diff_n result bB))))
          |(e::bA,_) -> tmp bA bB q (e::result)
      in tmp (reverse_list bA) (reverse_list bB) [] []

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = diff_b bA (mult_b (quot_b bA bB) bB)

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
  let tmp x y = (x,y)
  in tmp (quot_b bA bB) (mod_b bA bB)

