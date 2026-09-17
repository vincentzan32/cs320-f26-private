(* i used ai to help me with syntax and write parts of the code that didn't make sense to me, i first wrote most of the functions in python then translated parts i was unsure about*)

let taxicab (n : int) : int =
  let cube x = x * x * x in
  let rec inner a b =
    let s = cube a + cube b in
    if s = n then 1 + inner a (b + 1)
    else if s < n then inner a (b + 1)
    else 0
  in
  let rec outer a =
    if 2 * cube a > n then 0
    else inner a a + outer (a + 1)
  in
  outer 1

let rec drop_trailing (k : 'a) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs ->
    let xs' = drop_trailing k xs in
    if xs' = [] && x = k then []
    else x :: xs'

let every_k (k : int) (l : 'a list) : 'a list =
  let rec aux count l =
    match l with
    | [] -> []
    | x :: xs ->
      if count = 1 then x :: aux k xs
      else aux (count - 1) xs
  in
  aux 1 l

let is_bitonic (l : int list) : bool =
  let rec inc_only = function
    | a :: b :: rest -> a < b && inc_only (b :: rest)
    | _ -> true
  in
  let rec dec_only = function
    | a :: b :: rest -> a > b && dec_only (b :: rest)
    | _ -> true
  in
  let rec up = function
    | a :: b :: rest ->
      if a < b then up (b :: rest)
      else if a > b then dec_only (a :: b :: rest)
      else false
    | _ -> true
  in
  let rec down = function
    | a :: b :: rest ->
      if a > b then down (b :: rest)
      else if a < b then inc_only (a :: b :: rest)
      else false
    | _ -> true
  in
  match l with
  | [] | [_] -> true
  | a :: b :: _ -> if a < b then up l else down l

let factor (n : int) : (int * int) list =
  let rec count_factor n p =
    if n mod p = 0 then
      let (n', e) = count_factor (n / p) p in
      (n', e + 1)
    else
      (n, 0)
  in
  let rec go n p =
    if p * p > n then
      if n = 1 then [] else [(n, 1)]
    else
      let (n', e) = count_factor n p in
      if e = 0 then go n (p + 1)
      else (p, e) :: go n' (p + 1)
  in
  go n 2

type path = int * (bool * int) list

let is_valid (p1 : path) (p2 : path) : bool =
  let (s1, m1) = p1 and (s2, m2) = p2 in
  let step cur = function
    | None -> cur
    | Some (dir, dist) -> if dir then cur + dist else cur - dist
  in
  let hd = function [] -> None | x :: _ -> Some x in
  let tl = function [] -> [] | _ :: t -> t in
  let rec go cur1 cur2 m1 m2 diff =
    if diff = 0 then false
    else if m1 = [] && m2 = [] then true
    else
      let cur1' = step cur1 (hd m1) in
      let cur2' = step cur2 (hd m2) in
      let diff' = cur1' - cur2' in
      if diff' = 0 || diff' * diff < 0 then false
      else go cur1' cur2' (tl m1) (tl m2) diff'
  in
  go s1 s2 m1 m2 (s1 - s2)
