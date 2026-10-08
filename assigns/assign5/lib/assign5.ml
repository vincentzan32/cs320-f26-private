(* AI citation: I wrote my own solution and reasoning for this problem, then used Claude to find and fix bugs in my logic and code. *)

let group (l : int list) : int list list option =
  let rec go cur l =
    match l with
    | [] ->
      if cur = [] then Some [] else Some [List.rev cur]
    | 0 :: rest ->
      (match cur, rest with
       | c :: _, y :: _ when y <> 0 && c * y < 0 ->
         Option.map (fun gs -> List.rev cur :: gs) (go [] rest)
       | _ -> None)
    | x :: rest ->
      (match cur with
       | c :: _ when c * x < 0 -> None
       | _ -> go (x :: cur) rest)
  in
  go [] l

type 'a rtree = Node of 'a * 'a rtree list

let rec split (t : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  match t with
  | Node ((a, b), children) ->
    let pairs = List.map split children in
    let lefts = List.map (fun (l, _) -> l) pairs in
    let rights = List.map (fun (_, r) -> r) pairs in
    (Node (a, lefts), Node (b, rights))

let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go pre rest =
    match f (List.rev pre) with
    | Some b -> Some (b, rest)
    | None ->
      (match rest with
       | [] -> None
       | x :: xs -> go (x :: pre) xs)
  in
  go [] l
let apply_cycle (funcs : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let rec go fs n x =
    if n = 0 then x
    else
      match fs with
      | [] -> go funcs n x
      | f :: rest -> go rest (n - 1) (f x)
  in
  go funcs n x

let walks g len paths_starts =
  let rec walk p n x =
    if n = 0 then Some x
    else if g x (p x) then walk p (n - 1) (p x)
    else None
  in
  paths_starts
  |> List.map (fun (p, s) -> walk p len s)
  |> List.filter Option.is_some
  |> List.map (fun o -> match o with Some x -> x | None -> failwith "impossible")

  