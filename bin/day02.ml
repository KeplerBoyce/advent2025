let split_range str =
  let nums = String.split_on_char '-' str in
  match nums with
    | [] -> failwith "No numbers in range"
    | head :: tail -> match tail with
      | [] -> failwith "No numbers in range"
      | head2 :: _ -> (int_of_string head, int_of_string head2)

let sum_invalid (invalid_fn: string -> bool) count range =
  let (low, high) = range in
  let rec check_num value num_count =
    if value > high then
      num_count
    else
      let new_count =
        if invalid_fn (string_of_int value) then
          num_count + value
        else
          num_count
      in
      check_num (value + 1) new_count
  in
  let range_count = check_num low 0 in
  count + range_count

(* Part 1 *)
let () =
  let line = Util.read_first "inputs/day02" in
  let ranges = String.split_on_char ',' line in

  let is_invalid str =
    let len = String.length str in
    let mid = len / 2 in
    let first = String.sub str 0 mid in
    let second = String.sub str mid (len - mid) in
    first = second
  in

  let ranges = List.map split_range ranges in
  let fold_fn = sum_invalid is_invalid in
  let answer = List.fold_left fold_fn 0 ranges in
  print_endline (string_of_int answer)

(* Part 2 *)
let () =
  let line = Util.read_first "inputs/day02" in
  let ranges = String.split_on_char ',' line in

  let is_invalid2 str =
    let rec check_pattern length =
      let str_len = (String.length str) in
      if length > str_len / 2 then
        false
      else if (str_len mod length) <> 0 then
        check_pattern (length + 1)
      else

      let pattern = String.sub str 0 length in
      let rec advance pos =
        if pos >= str_len then
          true
        else

        let pos_pattern = String.sub str pos length in
        if pattern <> pos_pattern then
          false
        else
          advance (pos + length)
      in

      let invalid = advance length in
      if invalid then
        true
      else
        check_pattern (length + 1)
    in
    check_pattern 1
  in

  let ranges = List.map split_range ranges in
  let fold_fn = sum_invalid is_invalid2 in
  let answer = List.fold_left fold_fn 0 ranges in
  print_endline (string_of_int answer)
