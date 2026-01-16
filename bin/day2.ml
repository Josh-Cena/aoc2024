type diff_type = Inc of int | Dec of int | Invalid of int

let get_diff_type d =
  if 1 <= d && d <= 3 then Inc d
  else if -3 <= d && d <= -1 then Dec d
  else Invalid d

let is_inc d = match d with Inc _ -> true | _ -> false
let is_dec d = match d with Dec _ -> true | _ -> false
let is_invalid d = match d with Invalid _ -> true | _ -> false

let rec differences lst =
  match lst with
  | a :: b :: rest -> get_diff_type (b - a) :: differences (b :: rest)
  | _ -> []

let parse_line line = List.map int_of_string (String.split_on_char ' ' line)

let solve1 data =
  let is_safe lst =
    let diffs = differences lst in
    List.for_all is_inc diffs || List.for_all is_dec diffs
  in
  let safe_lines = List.filter is_safe (List.map parse_line data) in
  Printf.printf "%d\n" (List.length safe_lines)

let diff_groups diffs =
  let rec aux diffs (inc, dec, invalid) =
    match diffs with
    | (i, Inc d) :: rest -> aux rest ((i, d) :: inc, dec, invalid)
    | (i, Dec d) :: rest -> aux rest (inc, (i, d) :: dec, invalid)
    | (i, Invalid d) :: rest -> aux rest (inc, dec, (i, d) :: invalid)
    | [] -> (inc, dec, invalid)
  in
  aux (List.mapi (fun i x -> (i, x)) diffs) ([], [], [])

let solve2 data =
  let is_safe lst =
    let diffs = differences lst in
    let inc, dec, invalid = diff_groups diffs in
    match (inc, dec, invalid) with
    (* If there are two invalid then we must remove the middle *)
    | (_, [], [ (j, dj); (i, di) ] | _, [ (j, dj) ], [ (i, di) ])
      when j = i + 1 || j = i - 1 ->
        is_inc (get_diff_type (di + dj))
    | ([], _, [ (j, dj); (i, di) ] | [ (j, dj) ], _, [ (i, di) ])
      when j = i + 1 || j = i - 1 ->
        is_dec (get_diff_type (di + dj))
    (* If there is one invalid, remove either left or right *)
    | _, [ (i, di) ], [] | _, [], [ (i, di) ] ->
        i = 0
        || i = List.length lst - 2
        || is_inc (get_diff_type (List.assoc (i - 1) inc + di))
        || is_inc (get_diff_type (List.assoc (i + 1) inc + di))
    | [ (i, di) ], _, [] | [], _, [ (i, di) ] ->
        i = 0
        || i = List.length lst - 2
        || is_dec (get_diff_type (List.assoc (i - 1) dec + di))
        || is_dec (get_diff_type (List.assoc (i + 1) dec + di))
    (* If there are no invalid diffs, it's already safe *)
    | _, [], [] | [], _, [] -> true
    | _ -> false
  in
  let safe_lines = List.filter is_safe (List.map parse_line data) in
  Printf.printf "%d\n" (List.length safe_lines)
