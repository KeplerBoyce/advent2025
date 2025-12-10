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

  let merge_fn acc range =
    let (low, high) = range in
    let range_list = Util.list_range low high in
    Util.merge_no_duplicate acc range_list
  in
  let fresh = List.fold_left merge_fn [] ranges in
  let answer = List.length fresh in
  print_endline (string_of_int answer)
