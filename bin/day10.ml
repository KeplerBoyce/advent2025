(* Part 1 *)
let get_target line =
  let sections = String.split_on_char '[' line in
  let str = List.nth sections 1 in
  let str = List.hd (String.split_on_char ']' str) in
  
  let map_fn c =
    if c = '#' then 1
    else 0
  in
  List.map map_fn (Util.split str)

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

let apply_buttons num_lights buttons =
  let fold_fn acc button =
    let new_acc = List.map2 (+) acc button in
    List.map (fun x -> x mod 2) new_acc
  in
  let initial = List.init num_lights (fun _ -> 0) in
  List.fold_left fold_fn initial buttons

let () =
  let lines = Util.read_lines "inputs/day10" in
  let bignum = 1000000 in

  let process_line line =
    let target = get_target line in
    let num_lights = List.length target in
    let buttons = get_buttons num_lights line in
    let combos = Util.power_set buttons in
    
    let min_presses acc combo =
      let presses = List.length combo in
      if presses >= acc then acc
      else
      let final = apply_buttons num_lights combo in
      if final = target then presses
      else acc
    in
    List.fold_left min_presses bignum combos
  in
  let min_per_line = List.map process_line lines in
  let answer = Util.sum min_per_line in
  Util.print_int answer

(* Part 2 *)
let get_joltages line =
  let sections = String.split_on_char '{' line in
  let str = List.nth sections 1 in
  let str = List.hd (String.split_on_char '}' str) in
  let num_strs = String.split_on_char ',' str in
  List.map int_of_string num_strs

let overshot list =
  List.exists (fun x -> x < 0) list

let half list =
  if List.exists (fun x -> (x mod 2) = 1) list then
    None
  else
    Some (List.map (fun x -> x / 2) list)

let () =
  let lines = Util.read_lines "inputs/day10" in
  let bignum = 1000000 in

  let process_line i line =
    let () = Printf.printf "Progress: started #" in
    let () = Util.print_int i in
    let initial = get_joltages line in
    let num_lights = List.length initial in
    let buttons = get_buttons num_lights line in
    let combos = Util.power_set buttons in
    let target = List.init num_lights (fun _ -> 0) in
    
    let rec min_presses best curr presses =
      if presses >= best then best
      else
      let fold_fn acc combo =
        let sub_list list other =
          List.map2 (-) list other
        in
        let applied = List.fold_left sub_list curr combo in
        let combo_size = List.length combo in
        if combo_size >= acc then acc
        else if applied = target then combo_size
        else if overshot applied then acc
        else
        match half applied with
        | None -> acc
        | Some halved -> begin
            let presses_left = min_presses bignum halved 0 in
            min acc (2 * presses_left + combo_size)
          end
      in
      let best_left = List.fold_left fold_fn bignum combos in
      min best (presses + best_left)
    in
    min_presses bignum initial 0
  in
  let min_per_line = List.mapi process_line lines in
  let answer = Util.sum min_per_line in
  Util.print_int answer
