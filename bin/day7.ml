let solve1 data =
  let parse_line line =
    let parts = Str.split (Str.regexp ": ") line in
    match parts with
    | [res; rest] -> (int_of_string res, List.map int_of_string (String.split_on_char ' ' rest))
    | _ -> failwith "Invalid line format"
  in
  let rec can_calc res cur_res ops =
    match ops with
    | [] -> cur_res = res
    | op :: rest -> can_calc res (cur_res + op) rest || can_calc res (cur_res * op) rest
  in
  let valid_lines = List.filter_map (fun line ->
    match parse_line line with
    | (res, a :: ops) -> if can_calc res a ops then Some res else None
    | _ -> None
  ) data in
  Printf.printf "%d\n" (List.fold_left (+) 0 valid_lines)

let solve2 data =
  let parse_line line =
    let parts = Str.split (Str.regexp ": ") line in
    match parts with
    | [res; rest] -> (int_of_string res, List.map int_of_string (String.split_on_char ' ' rest))
    | _ -> failwith "Invalid line format"
  in
  let rec can_calc res cur_res ops =
    match ops with
    | [] -> cur_res = res
    | op :: rest -> can_calc res (cur_res + op) rest || can_calc res (cur_res * op) rest || can_calc res (int_of_string (string_of_int cur_res ^ string_of_int op)) rest
  in
  let valid_lines = List.filter_map (fun line ->
    match parse_line line with
    | (res, a :: ops) -> if can_calc res a ops then Some res else None
    | _ -> None
  ) data in
  Printf.printf "%d\n" (List.fold_left (+) 0 valid_lines)
