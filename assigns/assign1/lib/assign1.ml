(* wanted to note that i did use ai to help me understand and learn the syntax for ocaml, i wrote out all the solutions in python first which made understanding easier *)

let num_digits (n: int): int =
  let n = if n < 0 then n * -1 else n in
  let rec go n ans =
    if n > 9 then go (n / 10) (ans + 1) else ans
  in
  go n 1

let is_perfect_pow (_i: int) (_n: int): bool = (* CHANGE _i and _n to i and n *)
  assert false

let rec num_factors (n: int): int =
  let rec divide d = 
    if n mod d = 0 then 1 + num_factors (n / d)
    else divide (d + 1)
  in
  if n = 1 then 0 else divide 2

let is_hypotenuse (n: int): bool =
  let rec func_b a b =
    if b > n then false
    else if a * a + b * b = n * n then true
    else func_b a (b + 1)
  in
  let rec func_a a =
    if a > n then false
    else if func_b a a then true
    else func_a (a + 1)
  in
  func_a 1
  

let rec drop_leading (k: int) (l: int list): int list = 
  match l with
  | x :: xs when x = k -> drop_leading k xs
  | _ -> l