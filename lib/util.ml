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

let split_range str =
  let nums = String.split_on_char '-' str in
  match nums with
    | [] -> failwith "No numbers in range"
    | head :: tail -> match tail with
      | [] -> failwith "No numbers in range"
      | head2 :: _ -> (int_of_string head, int_of_string head2)

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

module UnordSet = struct
  type 'a t = ('a, unit) Hashtbl.t
  let create initial_size = Hashtbl.create initial_size
  let add set element = Hashtbl.add set element ()
  let mem set element = Hashtbl.mem set element
  let remove set element = Hashtbl.remove set element
  let size set = Hashtbl.fold (fun _ _ acc -> acc + 1) set 0
end

let add_list_to_set set list =
  let rec add_loop acc rem =
    match rem with
      | head :: tail ->
          let () = UnordSet.add acc head in
          add_loop acc tail
      | [] -> acc
  in
  add_loop set list
