(* Part 1 *)
let extend_beams prev curr =
  let len = String.length prev in

  let map_splits_fn i c =
    let prev_above = String.get prev i in
    if prev_above = '|' && c = '^' then '1'
    else '0'
  in

  let map_fn i c =
    let prev_above = String.get prev i in
    let prev_left = if i = 0 then '0'
    else String.get prev (i - 1) in
    let prev_right = if i = (len - 1) then '0'
    else String.get prev (i + 1) in

    let curr_left = if i = 0 then '0'
    else String.get curr (i - 1) in
    let curr_right = if i = (len - 1) then '0'
    else String.get curr (i + 1) in

    if c = '.' && prev_above = '|' then '|'
    else if curr_left = '^' && prev_left = '|' then '|'
    else if curr_right = '^' && prev_right = '|' then '|'
    else c
  in

  let splits = String.mapi map_splits_fn curr in
  let sum_fn a c =
    if c = '1' then a + 1
    else a
  in
  let num_splits = String.fold_left sum_fn 0 splits in
  let new_line = String.mapi map_fn curr in
  (num_splits, new_line)

let rec count_splits num_splits prev rem =
  match rem with
  | [] -> num_splits
  | curr :: tail ->
    let (line_splits, new_line) = extend_beams prev curr in
    count_splits (num_splits + line_splits) new_line tail
  
let () =
  let lines = Util.read_lines "inputs/day07" in
  let (head, tail) = match lines with
  | h :: t -> (h, t)
  | [] -> failwith "Input is empty"
  in

  let start_map_fn c =
    if c = 'S' then '|'
    else c
  in
  let start = String.map start_map_fn head in
  let answer = count_splits 0 start tail in
  print_endline (string_of_int answer)
