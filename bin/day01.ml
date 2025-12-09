(* Part 1 *)
let adjust (str, num) =
  let first, rest_str = Util.head_and_rest str in
  let amount = int_of_string rest_str in
  match first with
    | 'R' -> (num + amount) mod 100
    | 'L' -> (((num - amount) mod 100) + 100) mod 100
    | _ -> num

let () =
  let lines = Util.read_lines "inputs/day01" in
  let process_line (value, count) line =
    let result = adjust (line, value) in
    match result with
      | 0 -> (result, count + 1)
      | _ -> (result, count)
  in

  let (_, count) = List.fold_left process_line (50, 0) lines in
  print_endline (string_of_int count)

(* Part 2 *)
let adjust2 (str, num) =
  let first, rest_str = Util.head_and_rest str in
  let amount = int_of_string rest_str in
  let res = match first with
    | 'R' -> (num + amount) mod 100
    | 'L' -> (((num - amount) mod 100) + 100) mod 100
    | _ -> num
  in
  let to_zero = match first with
    | 'R' -> 100 - num
    | 'L' -> num
    | _ -> 0
  in
  let clicks =
    if to_zero = 0 then
      amount / 100
    else if amount >= to_zero then
      1 + (amount - to_zero) / 100
    else
      0
  in
  (res, clicks)

let () =
  let lines = Util.read_lines "inputs/day01" in
  let process_line (value, count) line =
    let (result, clicks) = adjust2 (line, value) in
    (result, count + clicks)
  in

  let (_, count) = List.fold_left process_line (50, 0) lines in
  print_endline (string_of_int count)
