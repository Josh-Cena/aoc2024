let solve1 data =
  let parse_line line =
    let parts = Str.split (Str.regexp ": ") line in
    match parts with
    | [ res; rest ] ->
        ( int_of_string res,
          List.map int_of_string (String.split_on_char ' ' rest) )
    | _ -> failwith "Invalid line format"
  in
  let rec can_calc cur_res ops =
    match ops with
    | [] -> failwith "No operators left"
    | [ op ] -> cur_res = op
    | op :: rest ->
        (op <> 0 && cur_res mod op = 0 && can_calc (cur_res / op) rest)
        || can_calc (cur_res - op) rest
  in
  let valid_lines =
    List.filter_map
      (fun line ->
        let res, ops = parse_line line in
        if can_calc res (List.rev ops) then Some res else None)
      data
  in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 valid_lines)

let solve2 data =
  let parse_line line =
    let parts = Str.split (Str.regexp ": ") line in
    match parts with
    | [ res; rest ] ->
        ( int_of_string res,
          List.map int_of_string (String.split_on_char ' ' rest) )
    | _ -> failwith "Invalid line format"
  in
  let rec can_calc cur_res ops =
    match ops with
    | [] -> failwith "No operators left"
    | [ op ] -> cur_res = op
    | op :: rest ->
        (op <> 0 && cur_res mod op = 0 && can_calc (cur_res / op) rest)
        || (let op_str = string_of_int op in
            let cur_str = string_of_int cur_res in
            String.length cur_str > String.length op_str
            && String.ends_with ~suffix:op_str cur_str
            &&
            let remaining_str =
              String.sub cur_str 0 (String.length cur_str - String.length op_str)
            in
            can_calc (int_of_string remaining_str) rest)
        || (cur_res > op && can_calc (cur_res - op) rest)
  in
  let valid_lines =
    List.filter_map
      (fun line ->
        let res, ops = parse_line line in
        if can_calc res (List.rev ops) then Some res else None)
      data
  in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 valid_lines)
