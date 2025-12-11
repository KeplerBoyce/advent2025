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

(* Part 2 *)
let () =
  let lines = Util.read_lines "inputs/day06" in
  let line_chars = List.map Util.split lines in
  let zipped = Util.zip line_chars in

  let get_num_lists char_list =
    let rec get_nums_loop num_list rem =
      match rem with
      | [] -> List.rev num_list
      | col :: rest ->
        let rec get_num acc rem =
          match rem with
          | [] -> acc
          | head :: tail ->
            if Util.is_digit head then
              let head = Util.int_of_char head in
              let acc = match acc with
              | None -> 0
              | Some x -> x
              in
              get_num (Some (acc * 10 + head)) tail
            else
              get_num acc tail
        in
        let num = get_num None col in
        let new_num_list = match num with
        | None -> [] :: num_list
        | Some n -> match num_list with
          | [] -> [[n]]
          | first :: rest -> (n :: first) :: rest
        in
        get_nums_loop new_num_list rest
    in
    get_nums_loop [] char_list
  in
  let num_lists = get_num_lists zipped in
  let last_line_chars = List.hd (List.rev line_chars) in
  let ops = Util.strip_spaces last_line_chars in

  let compute num_list op =
    if op = '+' then
      List.fold_left (fun a b -> a + b) 0 num_list
    else
      List.fold_left (fun a b -> a * b) 1 num_list
  in
  let computed = List.map2 compute num_lists ops in
  let answer = List.fold_left (fun a b -> a + b) 0 computed in
  print_endline (string_of_int answer)
