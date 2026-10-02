
let is_ws c = c = ' ' || c = '\n' || c = '\r' || c = '\t' || c = '\012'

let split_by_ws (s : string) : string list =
  let n = String.length s in
  let rec go i start =
    if i >= n then
      (if start < i then [String.sub s start (i - start)] else [])
    else if is_ws (String.get s i) then
      let rest = go (i + 1) (i + 1) in
      if start < i then String.sub s start (i - start) :: rest else rest
    else go (i + 1) start
  in
  go 0 0

type dir = N | S | E | W

let dist (dirs : dir list) : float =
  let rec go dirs x y =
    match dirs with
    | [] -> sqrt (float_of_int (x * x + y * y))
    | N :: r -> go r x (y + 1)
    | S :: r -> go r x (y - 1)
    | E :: r -> go r (x + 1) y
    | W :: r -> go r (x - 1) y
  in
  go dirs 0 0

type int_or_string
  = Int of int
  | String of string

type int_list_or_string_list
  = Int_list of int list
  | String_list of string list

let rec convert (l : int_or_string list) : int_list_or_string_list list =
  match l with
  | [] -> []
  | Int n :: rest ->
    (match convert rest with
     | Int_list ns :: r -> Int_list (n :: ns) :: r
     | r -> Int_list [n] :: r)
  | String s :: rest ->
    (match convert rest with
     | String_list ss :: r -> String_list (s :: ss) :: r
     | r -> String_list [s] :: r)

type 'a tree
  = Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert (x : 'a) (t : 'a tree) : 'a tree =
  match t with
  | Empty -> Node (x, Empty, Empty)
  | Node (v, l, r) ->
    if x < v then Node (v, insert x l, r)
    else Node (v, l, insert x r)

let rec flatten (t : 'a tree) : 'a list =
  match t with
  | Empty -> []
  | Node (v, l, r) -> flatten l @ (v :: flatten r)

let sort (l : 'a list) : 'a list =
  let rec build l t =
    match l with
    | [] -> t
    | x :: r -> build r (insert x t)
  in
  flatten (build l Empty)
