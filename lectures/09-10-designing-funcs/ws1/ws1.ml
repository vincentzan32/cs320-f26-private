let rec digits_of_int (n : int) : int list = 
  let rec loop n acc =
    if 0 <= n && n <= 9
    then n :: acc
    else loop (n / 10) (n mod 10 :: acc)
  in
  loop (abs n) []

let _ = assert(digits_of_int 123 = [1; 2; 3])