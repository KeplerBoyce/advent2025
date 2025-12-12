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
type dir =
  | Left
  | Right
  | Up
  | Down

let clockwise points =
  let shoelace_sum list =
    let (first_x, first_y), tail = match list with
    | h :: t -> (h, t)
    | [] -> failwith "Not enough points"
    in

    let rec loop acc prev_x rem =
      match rem with
      | [] -> acc + (prev_x * first_y)
      | (curr_x, curr_y) :: rest ->
        let new_acc = acc + (prev_x * curr_y) in
        loop new_acc curr_x rest
    in
    loop 0 first_x tail
  in
  let forward = shoelace_sum points in
  let backward = shoelace_sum (List.rev points) in
  let signed_area = forward - backward in
  signed_area < 0

let get_dir clockwise (x, y) (next_x, next_y) =
  if x = next_x then
    if y < next_y then
      if clockwise then Right else Left
    else
      if clockwise then Left else Right
  else
    if x < next_x then
      if clockwise then Down else Up
    else
      if clockwise then Up else Down

let on_correct_side clockwise points (x, y) (other_x, other_y) =
  let (next_x, next_y) = Util.next_cyclic points (x, y) in
  let (prev_x, prev_y) = Util.prev_cyclic points (x, y) in
  let next_dir = get_dir clockwise (x, y) (next_x, next_y) in
  let prev_dir = get_dir clockwise (prev_x, prev_y) (x, y) in

  let test_point dir (px, py) =
    let (low_x, high_x) = (min x other_x, max x other_x) in
    let (low_y, high_y) = (min y other_y, max y other_y) in
    if px < low_x || px > high_x || py < low_y || py > high_y then
      true
    else
    match dir with
    | Left -> other_x <= x
    | Right -> other_x >= x
    | Up -> other_y >= y
    | Down -> other_y <= y
  in
  test_point next_dir (next_x, next_y) &&
  test_point prev_dir (prev_x, prev_y)

let valid clockwise points ((x1, y1), (x2, y2)) =
  let inside (x, y) =
    let (low_x, high_x) = (min x1 x2, max x1 x2) in
    let (low_y, high_y) = (min y1 y2, max y1 y2) in
    x > low_x && x < high_x && y > low_y && y < high_y
  in
  let fold_fn acc point = acc || inside point in
  if List.fold_left fold_fn false points then false
  else
  on_correct_side clockwise points (x1, y1) (x2, y2) &&
  on_correct_side clockwise points (x2, y2) (x1, y1)

let () =
  let lines = Util.read_lines "inputs/day09test" in
  let points = List.map get_point lines in
  let pairs = Util.list_pairs points in
  let clockwise = clockwise points in
  let valid_pairs = List.filter (valid clockwise points) pairs in

  (* let print_pair ((x1, y1), (x2, y2)) = *)
  (*   let area = rect_area ((x1, y1), (x2, y2)) in *)
  (*   Printf.printf "(%d %d) (%d %d) %d\n" x1 y1 x2 y2 area *)
  (* in *)
  (* let () = List.iter print_pair valid_pairs in *)

  let max_area acc pair =
    let area = rect_area pair in
    max acc area
  in
  let answer = List.fold_left max_area 0 valid_pairs in
  Util.print_int answer
