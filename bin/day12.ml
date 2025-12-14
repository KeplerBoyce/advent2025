let get_shape section =
  let string_fold_fn acc c =
    if c = '#' then acc + 1
    else acc
  in
  let lines_fold_fn acc line =
    acc + String.fold_left string_fold_fn 0 line
  in
  let lines = List.drop 1 section in
  let num_squares = List.fold_left lines_fold_fn 0 lines in
  num_squares

let parse_query line =
  let (size_str, rest) = match String.split_on_char ':' line with
  | head :: next :: [] -> (head, next)
  | _ -> failwith "Invalid query line"
  in
  let dims = match String.split_on_char 'x' size_str with
  | head :: next :: [] -> (int_of_string head, int_of_string next)
  | _ -> failwith "Invalid query line"
  in
  let num_strs = List.drop 1 (String.split_on_char ' ' rest) in
  let nums = List.map int_of_string num_strs in
  (dims, nums)

let naive_solvable shapes (dims, nums) =
  let (width, height) = dims in
  let max_presents_naive = (width / 3) * (height / 3) in
  let square_counts = List.map2 (fun a b -> a * b) shapes nums in
  let total_squares = Util.sum square_counts in

  if Util.sum nums > max_presents_naive then false
  else if total_squares > width * height then false
  else true

let () =
  let lines = Util.read_lines "inputs/day12" in
  let line_sections = Util.split_blanks lines in
  let num_shapes = (List.length line_sections) - 1 in

  let shape_lines = List.take num_shapes line_sections in
  let query_lines = List.nth line_sections num_shapes in
  let shapes = List.map get_shape shape_lines in
  let queries = List.map parse_query query_lines in

  let solvable = List.map (naive_solvable shapes) queries in
  let counts = List.map (fun x -> if x then 1 else 0) solvable in
  let answer = Util.sum counts in
  Util.print_int answer
