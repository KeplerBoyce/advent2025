(* Part 1 *)
let () =
  let line = Util.read_first "inputs/day02" in
  let ranges = String.split_on_char ',' line in

  let split_range str =
    let nums = String.split_on_char '-' str in
    match nums with
      | [] -> failwith "No numbers in range"
      | head :: tail -> match tail with
        | [] -> failwith "No numbers in range"
        | head2 :: _ -> (int_of_string head, int_of_string head2)
  in

  let is_invalid str =
    let len = String.length str in
    let mid = len / 2 in
    let first = String.sub str 0 mid in
    let second = String.sub str mid (len - mid) in
    first = second
  in

  let sum_invalid count range =
    let (low, high) = range in
    let rec check_num value num_count =
      if value > high then
        num_count
      else
        let new_count =
          if is_invalid (string_of_int value) then
            num_count + value
          else
            num_count
        in
        check_num (value + 1) new_count
    in
    let range_count = check_num low 0 in
    count + range_count
  in

  let ranges = List.map (split_range) ranges in
  let answer = List.fold_left sum_invalid 0 ranges in
  print_endline (string_of_int answer)
