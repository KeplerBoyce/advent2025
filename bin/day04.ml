(* Part 1 *)
let check_paper paper r c =
  let rows = Array.length paper in
  let cols = String.length paper.(0) in

  if r >= 0 && r < rows && c >= 0 && c < cols
    && String.get paper.(r) c = '@' then 1
  else 0

let counts_init paper r c =
  let check = check_paper paper in
  if check r c = 0 then
    9
  else

  let upleft = check (r - 1) (c - 1) in
  let up = check (r - 1) c in
  let upright = check (r - 1) (c + 1) in
  let right = check r (c + 1) in
  let downright = check (r + 1) (c + 1) in
  let down = check (r + 1) c in
  let downleft = check (r + 1) (c - 1) in
  let left = check r (c - 1) in
  upleft + up + upright + right + downright + down + downleft + left

let num_adjacent lines =
  let rows = List.length lines in
  let cols = String.length (List.hd lines) in
  let paper = Array.of_list lines in
  Array.init_matrix rows cols (counts_init paper)

let count_accessible counts =
  let fold_rows acc row =
    let fold_row acc x =
      if x < 4 then acc + 1
      else acc
    in
    acc + Array.fold_left fold_row 0 row
  in
  Array.fold_left fold_rows 0 counts

let () =
  let lines = Util.read_lines "inputs/day04" in
  let counts = num_adjacent lines in
  let answer = count_accessible counts in
  print_endline (string_of_int answer)

(* Part 2 *)
let remove_papers paper counts =
  let map_rows i row =
    let map_row j char =
      if counts.(i).(j) < 4 then '.'
      else char
    in
    String.mapi map_row row
  in
  Array.mapi map_rows paper

let cycle_remove lines =
  let rows = List.length lines in
  let cols = String.length (List.hd lines) in
  let paper = Array.of_list lines in

  let rec cycle paper =
    let counts = Array.init_matrix rows cols (counts_init paper) in
    let new_paper = remove_papers paper counts in
    if paper = new_paper then
      0
    else
      count_accessible counts + cycle new_paper
  in
  cycle paper

let () =
  let lines = Util.read_lines "inputs/day04" in
  let answer = cycle_remove lines in
  print_endline (string_of_int answer)
