(* Part 1 *)
let get_target line =
  let sections = String.split_on_char '[' line in
  let str = List.nth sections 1 in
  let str = List.hd (String.split_on_char ']' str) in
  let num_lights = String.length str in
  
  let map_fn c =
    if c = '#' then '1'
    else '0'
  in
  let str = List.map map_fn (Util.split str) in
  let str = Util.merge (List.rev str) in
  (int_of_string ("0b" ^ str), num_lights)

let get_buttons num_lights line =
  let sections = String.split_on_char '(' line in
  let sections = List.drop 1 sections in

  let map_button_strs str =
    List.hd (String.split_on_char ')' str)
  in
  let buttons = List.map map_button_strs sections in
  let buttons = List.map (String.split_on_char ',') buttons in
  let buttons = List.map (List.map int_of_string) buttons in
  
  let map_nums button_list =
    let rec loop acc i rem =
      if i = num_lights then acc
      else
      match rem with
      | [] -> loop (acc * 2) (i + 1) []
      | head :: tail ->
        if i = head then
          loop (acc * 2 + 1) (i + 1) tail
        else
          loop (acc * 2) (i + 1) (head :: tail)
    in
    loop 0 0 button_list
  in
  List.map map_nums buttons

let apply_buttons buttons =
  let apply acc mask = acc lxor mask in
  List.fold_left apply 0 buttons

let () =
  let lines = Util.read_lines "inputs/day10" in
  let bignum = 1000000 in

  let process_line line =
    let (target, num_lights) = get_target line in
    let buttons = get_buttons num_lights line in
    let button_combos = Util.power_set buttons in
    
    let min_presses acc button_combo =
      let presses = List.length button_combo in
      if presses >= acc then acc
      else
      let final = apply_buttons button_combo in
      if final = target then presses
      else acc
    in
    List.fold_left min_presses bignum button_combos
  in
  let min_per_line = List.map process_line lines in
  let answer = Util.sum min_per_line in
  Util.print_int answer

(* Part 2 *)
let get_target2 line =
  let sections = String.split_on_char '{' line in
  let str = List.nth sections 1 in
  let str = List.hd (String.split_on_char '}' str) in
  let num_strs = String.split_on_char ',' str in
  let nums = List.map int_of_string num_strs in
  let num_lights = List.length num_strs in
  (nums, num_lights)

let get_buttons2 num_lights line =
  let sections = String.split_on_char '(' line in
  let sections = List.drop 1 sections in

  let map_button_strs str =
    List.hd (String.split_on_char ')' str)
  in
  let buttons = List.map map_button_strs sections in
  let buttons = List.map (String.split_on_char ',') buttons in
  let buttons = List.map (List.map int_of_string) buttons in
  
  let map_nums button_list =
    let rec loop acc i rem =
      if i = num_lights then List.rev acc
      else
      match rem with
      | [] -> loop (0 :: acc) (i + 1) []
      | head :: tail ->
        if i = head then
          loop (1 :: acc) (i + 1) tail
        else
          loop (0 :: acc) (i + 1) (head :: tail)
    in
    loop [] 0 button_list
  in
  List.map map_nums buttons

let overshot list target =
  let diffs = List.map2 (fun a b -> a - b) list target in
  let fold_fn acc diff =
    if acc then true
    else diff > 0
  in
  List.fold_left fold_fn false diffs

let () =
  let lines = Util.read_lines "inputs/day10" in
  let bignum = 1000000 in

  let process_line line =
    let (target, num_lights) = get_target2 line in
    let buttons = get_buttons2 num_lights line in
    
    let rec min_presses best so_far presses rem =
      if presses >= best then best
      else if overshot so_far target then best
      else if so_far = target then presses
      else
      match rem with
      | [] -> best
      | head :: tail ->
        let new_so_far = List.map2 (fun a b -> a + b) so_far head in
        let with_press = min_presses best new_so_far (presses + 1) (head :: tail) in
        let without_press = min_presses best so_far presses tail in
        min with_press without_press
    in
    let initial = List.init num_lights (fun _ -> 0) in
    min_presses bignum initial 0 buttons
  in
  let min_per_line = List.map process_line lines in
  let answer = Util.sum min_per_line in
  Util.print_int answer
