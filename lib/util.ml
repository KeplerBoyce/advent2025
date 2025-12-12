let read_lines filename =
  let ic = open_in filename in
  let rec read_loop acc =
    try
      let line = input_line ic in
      read_loop (line :: acc)
    with
      | End_of_file -> close_in ic; List.rev acc
  in read_loop []

let read_first filename =
  let lines = read_lines filename in
  match lines with
    | [] -> failwith "No lines in input file"
    | head :: _ -> head

let split_blank_line lines =
  let rec loop front rem =
    let line, rest = match rem with
      | h :: t -> (h, t)
      | [] -> failwith "Reached end of lines list"
    in
    if (String.length) line = 0 then
      (List.rev front, rest)
    else
      loop (line :: front) rest
  in
  loop [] lines

let split s =
  List.init (String.length s) (String.get s)

let head_and_rest s =
  match split s with
    | first :: rest ->
      let rest_str = List.to_seq rest |> String.of_seq in
      (first, rest_str)
    | _ -> (' ', "")

let int_of_char c =
  (Char.code c) - (Char.code '0')

let char_of_int x =
  Char.chr (Char.code '0' + x)

let is_digit c =
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'

let split_range str =
  let nums = String.split_on_char '-' str in
  match nums with
    | [] -> failwith "No numbers in range"
    | head :: tail -> match tail with
      | [] -> failwith "No numbers in range"
      | head2 :: _ -> (int_of_string head, int_of_string head2)

let split_spaces str =
  let rec loop list rem =
    if String.length rem = 0 then
      List.rev list
    else
    match (String.index_from_opt rem 0 ' ') with
    | None ->
        loop (rem :: list) ""
    | Some index ->
        let keep = String.sub rem 0 index in
        let len = String.length rem in
        let rest = String.sub rem index (len - index) in
        loop (keep :: list) (String.trim rest)
  in
  loop [] str

let strip_spaces char_list =
  let rec loop list rem =
    match rem with
    | [] -> List.rev list
    | head :: tail -> 
      if head = ' ' then
        loop list tail
      else
        loop (head :: list) tail
  in
  loop [] char_list

let zip lists =
  let rec process_list acc rem =
    match rem with
    | [] -> List.map (List.rev) acc
    | list :: rest ->
        let new_acc = List.map2 (fun e l -> e :: l) list acc in
        process_list new_acc rest
  in
  let empty = List.map (fun _ -> []) (List.hd lists) in
  process_list empty lists

let add_no_duplicate list item =
  if List.exists (fun x -> x = item) list then
    list
  else
    item :: list

let merge_no_duplicate list1 list2 =
  let rec merge_loop acc rem =
    match rem with
      | head :: tail ->
          let merged = add_no_duplicate acc head in
          merge_loop merged tail
      | [] -> acc
  in
  merge_loop list1 list2

let list_range low high =
  let rec loop i acc =
    if i > high then
      List.rev acc
    else
      loop (i + 1) (i :: acc)
  in
  loop low []

let sum list = List.fold_left (fun a b -> a + b) 0 list

let sort_up list = List.sort (fun a b -> a - b) list

let sort_down list = List.sort (fun a b -> b - a) list

let print_int_list list =
  let rec loop str rem =
    match rem with
    | head :: tail ->
      let new_str = if str = "" then
        "{ " ^ (string_of_int head)
      else
        str ^ ", " ^ (string_of_int head) in
      loop new_str tail
    | [] -> str ^ " }"
  in
  let str = loop "" list in
  Printf.printf "%s\n" str

let print_int x = print_endline (string_of_int x)

let list_pairs in_list =
  let rec loop list =
    match list with
    | [] -> []
    | head :: tail ->
        let pairs_with_head = List.map (fun other -> (head, other)) tail in
        let rem_pairs = loop tail in
        pairs_with_head @ rem_pairs
  in
  loop in_list

let wrap_list list =
  List.rev ((List.hd list) :: (List.rev list))

let next_cyclic list elem =
  let wrapped = wrap_list list in
  let rec loop rem =
    match rem with
    | head :: next :: tail ->
      if head = elem then next
      else loop (next :: tail)
    | _ -> failwith "Not found"
  in
  loop wrapped

let prev_cyclic list elem =
  next_cyclic (List.rev list) elem
