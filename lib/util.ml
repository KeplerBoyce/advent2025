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
