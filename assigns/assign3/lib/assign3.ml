(*
AI Use Disclosure: For the OCaml exercises (nub and split_by_ws'), I wrote initial attempts and then used Claude (Anthropic) to check my work. Claude also provided example implementations, which I compared against my own, asked follow-up questions about, and used to refine my final solutions, including verifying that they only use functions allowed by Stdlib320.
*)


let rec remove_key a b =
  match b with
  | [] -> []
  | ((a', _) as pair) :: rest ->
    if a' = a
    then remove_key a rest
    else pair :: remove_key a rest

let rec nub l =
  match l with
  | [] -> []
  | ((k, _) as pair) :: rest -> pair :: nub (remove_key k rest)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then acc
    else loop (s.[i] :: acc) (i + 1)
  in List.rev (loop [] 0)

let implode (l : char list) : string =
  String.init (List.length l) (List.nth l)

let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

let rec take_word l =
  match l with
  | c :: rest when not (is_ws c) ->
    let (word, remaining) = take_word rest in
    (c :: word, remaining)
  | _ -> ([], l)

let rec split l =
  match l with
  | [] -> []
  | c :: rest when is_ws c -> split rest
  | _ ->
    let (word, remaining) = take_word l in
    implode word :: split remaining

let split_by_ws' s = split (explode s)