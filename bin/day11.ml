(* Part 1 *)
let build_adj_list map line =
  let split_sides = String.split_on_char ':' line in
  let (node, others) = match split_sides with
  | node :: rest :: [] -> begin
      let other_strs = String.split_on_char ' ' rest in
      (node, List.drop 1 other_strs)
    end
  | _ -> failwith "Invalid line in input"
  in
  let () = Hashtbl.add map node others in
  ()

let () =
  let lines = Util.read_lines "inputs/day11" in
  let map = Hashtbl.create 10 in
  let () = List.iter (build_adj_list map) lines in
  
  let rec step curr =
    if curr = "out" then 1
    else
    let others = Hashtbl.find map curr in
    let fold_fn acc other = acc + step other in
    List.fold_left fold_fn 0 others
  in
  let answer = step "you" in
  Util.print_int answer

(* Part 2 *)
let () =
  let lines = Util.read_lines "inputs/day11" in
  let map = Hashtbl.create 10 in
  let () = List.iter (build_adj_list map) lines in
  let memo = Hashtbl.create 10 in
  
  let rec step dac fft curr =
    if curr = "out" then
      if dac && fft then 1
      else 0
    else if Hashtbl.mem memo (curr, dac, fft) then
      Hashtbl.find memo (curr, dac, fft)
    else
    let dac = dac || (curr = "dac") in
    let fft = fft || (curr = "fft") in
    let others = Hashtbl.find map curr in
    let fold_fn acc other = acc + (step dac fft other) in
    let ways = List.fold_left fold_fn 0 others in
    let () = Hashtbl.add memo (curr, dac, fft) ways in
    ways
  in
  let answer = step false false "svr" in
  Util.print_int answer
