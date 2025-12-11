(* Part 1 *)
let split_trio str =
  let sections = String.split_on_char ',' str in
  match sections with
  | [a; b; c] -> (int_of_string a, int_of_string b, int_of_string c)
  | _ -> failwith "Trio must have exactly 3 sections"

let dist_3d point1 point2 =
  let (x1, y1, z1) = point1 in
  let (x2, y2, z2) = point2 in
  let (dx, dy, dz) = (x1 - x2, y1 - y2, z1 - z2) in
  sqrt (float_of_int (dx * dx + dy * dy + dz * dz))

let sort_trio_pairs trio_pairs =
  let sort_fn pair1 pair2 =
    let ((_, point1), (_, point2)) = pair1 in
    let ((_, point3), (_, point4)) = pair2 in
    let dist1 = dist_3d point1 point2 in
    let dist2 = dist_3d point3 point4 in
    if dist1 < dist2 then -1
    else if dist1 > dist2 then 1
    else 0
  in
  List.sort sort_fn trio_pairs

let rec find parent i =
  if parent.(i) = i then i
  else begin
    let root = find parent parent.(i) in
    parent.(i) <- root;
    root
  end

let union parent size i j =
  let root_i = find parent i in
  let root_j = find parent j in

  if root_i <> root_j then begin
    let size_i = size.(root_i) in
    let size_j = size.(root_j) in
    parent.(root_i) <- root_j;
    size.(root_j) <- size_i + size_j;
    size.(root_j)
  end
  else 0

let () =
  let lines = Util.read_lines "inputs/day08" in
  let trios = List.map split_trio lines in
  let trios_idx = List.mapi (fun i x -> (i, x)) trios in
  let trio_pairs = Util.list_pairs trios_idx in
  let sorted = sort_trio_pairs trio_pairs in
  let connections = List.take 1000 sorted in

  let extract_indices pair =
    let ((idx1, _), (idx2, _)) = pair in
    (idx1, idx2)
  in
  let conn_idx = List.map extract_indices connections in

  let parent = Array.init 1000 (fun i -> i) in
  let size = Array.init 1000 (fun _ -> 1) in

  let () = List.iter (fun (i, j) -> ignore (union parent size i j)) conn_idx in
  let rec collect_sizes acc i =
    if i = 1000 then acc
    else if parent.(i) = i then
      collect_sizes (size.(i) :: acc) (i + 1)
    else
      collect_sizes acc (i + 1)
  in
  let sizes = collect_sizes [] 0 in
  let sorted_sizes = List.sort (fun a b -> b - a) sizes in
  let (a, b, c) = match (List.take 3 sorted_sizes) with
  | [a; b; c] -> (a, b, c)
  | _ -> failwith "Must be at least 3 groups"
  in
  let answer = a * b * c in
  Util.print_int answer

(* Part 2 *)
let () =
  let lines = Util.read_lines "inputs/day08" in
  let trios = List.map split_trio lines in
  let trios_idx = List.mapi (fun i x -> (i, x)) trios in
  let trio_pairs = Util.list_pairs trios_idx in
  let connections = sort_trio_pairs trio_pairs in
  
  let parent = Array.init 1000 (fun i -> i) in
  let size = Array.init 1000 (fun _ -> 1) in

  let rec add_edge_loop rem =
    let (edge, rest) = match rem with
    | head :: tail -> (head, tail)
    | [] -> failwith "Out of edges"
    in
    let ((idx1, point1), (idx2, point2)) = edge in
    let size = union parent size idx1 idx2 in
    if size = 1000 then begin
      let (x1, _, _) = point1 in
      let (x2, _, _) = point2 in
      x1 * x2
    end
    else add_edge_loop rest
  in
  let answer = add_edge_loop connections in
  Util.print_int answer
