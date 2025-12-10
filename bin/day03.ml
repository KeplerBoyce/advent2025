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

(* Part 2 *)
let get_max_joltage2 total_digit_num bank =
  let rec try_digit str so_far digit digit_num =
    let len = String.length str in
    if digit_num = total_digit_num then
      (so_far, true)
    else if len = 0 then
      (so_far, false)
    else if (total_digit_num - digit_num) > len then
      (so_far, false)
    else
    
    let c = Util.char_of_int digit in
    let index =
      match (String.index_from_opt str 0 c) with
        | Some idx -> idx
        | None -> -1
    in
    if index = -1 then
      try_digit str so_far (digit - 1) digit_num
    else

    let new_so_far = so_far * 10 + digit in
    let after_str = String.sub str (index + 1) (len - index - 1) in

    let next_res = try_digit after_str new_so_far 9 (digit_num + 1) in
    let (next_pos, next_full) = next_res in
    if next_full then
      next_res
    else

    if digit > 1 then
      let same_res = try_digit str so_far (digit - 1) digit_num in
      let (same_pos, _) = same_res in
      if same_pos > next_pos then
        same_res
      else
        next_res
    else
      next_res
  in
  let (joltage, _) = try_digit bank 0 9 0 in
  joltage

let () =
  let lines = Util.read_lines "inputs/day03" in
  let max_joltages = List.map (get_max_joltage2 12) lines in
  let answer = List.fold_left (fun a x -> a + x) 0 max_joltages in
  print_endline (string_of_int answer)
