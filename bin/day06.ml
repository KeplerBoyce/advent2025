(* Part 1 *)
let () =
  let lines = Util.read_lines "inputs/day06" in
  let lines_split = List.map Util.split_spaces lines in
  let zipped = Util.zip lines_split in
  let zipped_rev = List.map List.rev zipped in
  
  let compute list =
    match list with
    | op :: nums ->
        let nums = List.map (fun x -> int_of_string x) nums in
        if op = "+" then
          List.fold_left (fun a b -> a + b) 0 nums
        else
          List.fold_left (fun a b -> a * b) 1 nums
    | [] -> failwith "Empty math problem"
  in
  let computed = List.map compute zipped_rev in
  let answer = List.fold_left (fun a b -> a + b) 0 computed in
  print_endline (string_of_int answer)
