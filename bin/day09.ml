(* Part 1 *)
let get_point str =
  let sections = String.split_on_char ',' str in
  match sections with
  | [a; b] -> (int_of_string a, int_of_string b)
  | _ -> failwith "Invalid string for point"

let rect_area (point1, point2) =
  let (x1, y1) = point1 in
  let (x2, y2) = point2 in
  (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let () =
  let lines = Util.read_lines "inputs/day09" in
  let points = List.map get_point lines in
  let pairs = Util.list_pairs points in

  let max_area acc pair =
    let area = rect_area pair in
    max acc area
  in
  let answer = List.fold_left max_area 0 pairs in
  Util.print_int answer

(* Part 2 *)
let get_edge_tiles points =
  let rec loop acc rem =
    match rem with
    | (x, y) :: (next_x, next_y) :: rest ->
      let new_coords = if x = next_x then
        let (low_y, high_y) = (min y next_y, max y next_y) in
        let y_list = Util.list_range (low_y + 1) (high_y - 1) in
        List.map (fun y_val -> (x, y_val)) y_list
      else
        let (low_x, high_x) = (min x next_x, max x next_x) in
        let x_list = Util.list_range (low_x + 1) (high_x - 1) in
        List.map (fun x_val -> (x_val, y)) x_list
      in
      loop (new_coords :: acc) ((next_x, next_y) :: rest)
    | _ -> acc
  in
  loop [] (Util.wrap_list points)

let valid points ((x1, y1), (x2, y2)) =
  let inside (x, y) =
    let (low_x, high_x) = (min x1 x2, max x1 x2) in
    let (low_y, high_y) = (min y1 y2, max y1 y2) in
    x > low_x && x < high_x && y > low_y && y < high_y
  in

  let fold_fn acc point = acc || (inside point) in
  if List.fold_left fold_fn false points then
    false
  else

  let edge_lists = get_edge_tiles points in
  let edge_fold_fn acc edge_list =
    acc || (List.fold_left fold_fn false edge_list)
  in
  not (List.fold_left edge_fold_fn false edge_lists)

let () =
  let lines = Util.read_lines "inputs/day09" in
  let points = List.map get_point lines in
  let pairs = Util.list_pairs points in

  let max_valid_area acc pair =
    let area = rect_area pair in
    if acc > area then
      acc
    else if valid points pair then
      area
    else
      acc
  in
  let answer = List.fold_left max_valid_area 0 pairs in
  Util.print_int answer
