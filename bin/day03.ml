(* Part 1 *)
let get_max_digit str =
  let len = String.length str in
  let rec advance curr_max index =
    if index = len then
      curr_max
    else

    let curr = Util.int_of_char (String.get str index) in
    if curr > curr_max then
      advance curr (index + 1)
    else
      advance curr_max (index + 1)
  in
  advance 0 0

let get_max_joltage bank =
  let rec advance curr_max digit =
    if digit = 10 then
      curr_max
    else

    let digit_char = Util.char_of_int digit in
    let index =
      match (String.index_from_opt bank 0 digit_char) with
        | Some idx -> idx
        | None -> -1
    in
    if index = -1 then
      advance curr_max (digit + 1)
    else

    let len = String.length bank in
    let after_str = String.sub bank (index + 1) (len - index - 1) in
    let max_after = get_max_digit after_str in
    let curr_joltage = if max_after = 0 then
      digit
    else
      digit * 10 + max_after
    in

    if curr_joltage > curr_max then
      advance curr_joltage (digit + 1)
    else
      advance curr_max (digit + 1)
  in
  advance 0 1

let () =
  let lines = Util.read_lines "inputs/day03" in
  let max_joltages = List.map get_max_joltage lines in
  let answer = List.fold_left (fun a x -> a + x) 0 max_joltages in
  print_endline (string_of_int answer)
