(* Part 1 *)
let check_fresh ranges query =
  let in_range x range =
    let (first, second) = range in
    x >= first && x <= second
  in
  if List.exists (in_range query) ranges then 1
  else 0

let () =
  let lines = Util.read_lines "inputs/day05" in
  let (ranges, queries) = Util.split_blank_line lines in
  let ranges = List.map Util.split_range ranges in
  let queries = List.map (fun x -> (int_of_string x)) queries in
  let fresh = List.map (check_fresh ranges) queries in
  let answer = List.fold_left (fun a b -> a + b) 0 fresh in
  print_endline (string_of_int answer)

(* Part 2 *)
let () =
  let lines = Util.read_lines "inputs/day05" in
  let (ranges, _) = Util.split_blank_line lines in
  let ranges = List.map Util.split_range ranges in

  let compare range1 range2 =
    let (low1, _) = range1 in
    let (low2, _) = range2 in
    low1 - low2
  in
  let sorted = List.sort compare ranges in

  let rec num_unique (num, prev) rem =
    let (prev_low, prev_high) = prev in
    match rem with
    | [] -> num + prev_high - prev_low + 1
    | head :: tail ->
        let (low, high) = head in
        if low <= prev_high then
          let next = (prev_low, max prev_high high) in
          num_unique (num, next) tail
        else
          let added = prev_high - prev_low + 1 in
          num_unique (num + added, head) tail
  in

  let (first, rem) = match sorted with
  | head :: tail -> (head, tail)
  | [] -> failwith "Nothing in sorted ranges list"
  in
  let answer = num_unique (0, first) rem in
  print_endline (string_of_int answer)
