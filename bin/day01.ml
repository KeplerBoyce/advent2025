let split s =
  List.init (String.length s) (String.get s)

let get_parts s =
  match split s with
    | first :: rest ->
      let rest_str = List.to_seq rest |> String.of_seq in
      (first, rest_str)
    | _ -> (' ', "")

let adjust (str, num) =
  let first, rest_str = get_parts str in
  let amount = int_of_string rest_str in
  match first with
    | 'R' -> (num + amount) mod 100
    | 'L' -> (((num - amount) mod 100) + 100) mod 100
    | _ -> num

let read_lines filename =
  let ic = open_in filename in
  let rec read_loop acc =
    try
      let line = input_line ic in
      read_loop (line :: acc)
  with
    | End_of_file -> close_in ic; List.rev acc
  in read_loop []

(* Part 1 *)
let () =
  let filename = "inputs/day01" in
  let lines = read_lines filename in
  let process_line (value, count) line =
    let result = adjust (line, value) in
    match result with
      | 0 -> (result, count + 1)
      | _ -> (result, count)
  in

  let (_, count) = List.fold_left process_line (50, 0) lines in
  print_endline (string_of_int count)

let adjust2 (str, num) =
  let first, rest_str = get_parts str in
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

(* Part 2 *)
let () =
  let filename = "inputs/day01" in
  let lines = read_lines filename in
  let process_line (value, count) line =
    let (result, clicks) = adjust2 (line, value) in
    (result, count + clicks)
  in

  let (_, count) = List.fold_left process_line (50, 0) lines in
  print_endline (string_of_int count)
