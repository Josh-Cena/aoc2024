type node =
  | Input of string
  | Xor of string * node * node
  | And of string * node * node
  | Or of string * node * node

let rec string_of_tree node =
  match node with
  | Input name -> name
  | And (_, x, y) -> Printf.sprintf "(%s & %s)" (string_of_tree x) (string_of_tree y)
  | Or (_, x, y) -> Printf.sprintf "(%s | %s)" (string_of_tree x) (string_of_tree y)
  | Xor (_, x, y) -> Printf.sprintf "(%s ^ %s)" (string_of_tree x) (string_of_tree y)

let rec make_tree name (inputs, circuit) =
  if Hashtbl.mem inputs name then
    Input name
  else
    let (x, op, y) = Hashtbl.find circuit name in
    match op with
    | "AND" ->
      let l = make_tree x (inputs, circuit) in
      let r = make_tree y (inputs, circuit) in
      let lstr = string_of_tree l in
      let rstr = string_of_tree r in
      if String.compare lstr rstr < 0 then And (name, l, r) else And (name, r, l)
    | "OR" ->
      let l = make_tree x (inputs, circuit) in
      let r = make_tree y (inputs, circuit) in
      let lstr = string_of_tree l in
      let rstr = string_of_tree r in
      if String.compare lstr rstr < 0 then Or (name, l, r) else Or (name, r, l)
    | "XOR" ->
      let l = make_tree x (inputs, circuit) in
      let r = make_tree y (inputs, circuit) in
      let lstr = string_of_tree l in
      let rstr = string_of_tree r in
      if String.compare lstr rstr < 0 then Xor (name, l, r) else Xor (name, r, l)
    | _ -> failwith "Invalid operation"

let rec eval_tree inputs node =
  match node with
  | Input name -> Hashtbl.find inputs name
  | And (_, x, y) -> eval_tree inputs x && eval_tree inputs y
  | Or (_, x, y) -> eval_tree inputs x || eval_tree inputs y
  | Xor (_, x, y) -> eval_tree inputs x <> eval_tree inputs y

let rec tree_eq node1 node2 =
  match node1, node2 with
  | Input n1, Input n2 -> n1 = n2
  | And (_, x1, y1), And (_, x2, y2) ->
    tree_eq x1 x2 && tree_eq y1 y2 || tree_eq x1 y2 && tree_eq y1 x2
  | Or (_, x1, y1), Or (_, x2, y2) ->
    tree_eq x1 x2 && tree_eq y1 y2 || tree_eq x1 y2 && tree_eq y1 x2
  | Xor (_, x1, y1), Xor (_, x2, y2) ->
    tree_eq x1 x2 && tree_eq y1 y2 || tree_eq x1 y2 && tree_eq y1 x2
  | _ -> false

let make_trees data swaps =
  let chunks = Str.split (Str.regexp "\n\n") (String.concat "\n" data) in
  let inputs = List.nth chunks 0
    |> String.split_on_char '\n'
    |> List.map (Str.split (Str.regexp ": "))
    |> List.map (fun x -> (List.nth x 0, int_of_string (List.nth x 1) == 1))
    |> List.to_seq
    |> Hashtbl.of_seq in
  let circuit = List.nth chunks 1
    |> String.split_on_char '\n'
    |> List.map (Str.split (Str.regexp " -> "))
    |> List.map (fun x ->
      let parts = Str.split (Str.regexp " ") (List.nth x 0) in
      let name = List.nth x 1 in
      let name = Option.fold ~none:name ~some:(fun x -> x) (Hashtbl.find_opt swaps name) in
      match parts with
      | [x; y; z] -> (name, (x, y, z))
      | _ -> failwith "Invalid input")
    |> List.to_seq
    |> Hashtbl.of_seq in
  (inputs,
    Hashtbl.fold (fun k _ acc ->
      if String.starts_with ~prefix:"z" k then k :: acc else acc) circuit []
    |> List.sort String.compare
    |> List.rev
    |> List.map (fun x -> make_tree x (inputs, circuit)))

let solve1 data =
  let inputs, trees = make_trees data (Hashtbl.create 0) in
  let outputs = List.map (eval_tree inputs) trees in
  Printf.printf "%d\n" (List.fold_left (fun acc x -> acc * 2 + if x then 1 else 0) 0 outputs)

let name_of tree =
  match tree with
  | Input name -> name
  | And (name, _, _) -> name
  | Or (name, _, _) -> name
  | Xor (name, _, _) -> name

let rec register_op op_to_id tree =
  match tree with
  | Input _ -> ()
  | And (name, x, y) ->
    let op = (Printf.sprintf "%s & %s" (name_of x) (name_of y)) in
    Hashtbl.replace op_to_id op name;
    register_op op_to_id x;
    register_op op_to_id y
  | Or (name, x, y) ->
    let op = (Printf.sprintf "%s | %s" (name_of x) (name_of y)) in
    Hashtbl.replace op_to_id op name;
    register_op op_to_id x;
    register_op op_to_id y
  | Xor (name, x, y) ->
    let op = (Printf.sprintf "%s ^ %s" (name_of x) (name_of y)) in
    Hashtbl.replace op_to_id op name;
    register_op op_to_id x;
    register_op op_to_id y

let find_key_by_left key_prefix op_to_id =
  Hashtbl.fold (fun k v acc ->
    if String.starts_with ~prefix:key_prefix k then
      let prelen = String.length key_prefix in
      Some (String.sub k prelen (String.length k - prelen), v)
    else
      acc) op_to_id None

let find_key_by_right key_suffix op_to_id =
  Hashtbl.fold (fun k v acc ->
    if String.ends_with ~suffix:key_suffix k then
      let suflen = String.length key_suffix in
      Some (String.sub k 0 (String.length k - suflen), v)
    else
      acc) op_to_id None

(* For each adder zn, it receives, in order: carry(n-1), xn, yn.
   The graph is structured such that
   - foo = xn ^ yn
   - bar = xn & yn
   - baz = carry(n-1) & foo
   - carryn = baz | bar
   - zn = carryn ^ foo
   We just need to check if zn matches this structure.
*)
let solve2 data =
  (* I haven't given it enough thought to know to how to do it automatically *)
  let swaps = Hashtbl.of_seq (List.to_seq [
    ("z07", "rts"); ("rts", "z07"); (* rts = carry07 *)
    ("z12", "jpj"); ("jpj", "z12"); (* jpj = baz12 *)
    ("z26", "kgj"); ("kgj", "z26"); (* kgj = bar26 *)
    ("vvw", "chv"); ("chv", "vvw"); (* chv = foo34, vvw = bar34 *)
  ]) in
  let _, trees = make_trees data swaps in
  let op_to_id = Hashtbl.create (List.length data) in
  List.iter (register_op op_to_id) trees;
  let find_output_with_reparation op1 op op2 =
    begin match Hashtbl.find_opt op_to_id (Printf.sprintf "%s %s %s" op1 op op2) with
    (* If "op1 op op2" exists, then we assume that op1 and op2 are wired correctly.
       This is not safe but it suffices *)
    | Some id -> id
    (* If "op1 op op2" doesn't exist but "somethingElse op op2" does, then
      we know that op1 somethingElse has been swapped. It's entirely possible that
      actually op2 op somethingElse exists instead, but our sorting of the operands
      help to prevent this. *)
    | None ->
      let op2_swap = find_key_by_left (Printf.sprintf "%s %s " op1 op) op_to_id in
      let op1_swap = find_key_by_right (Printf.sprintf " %s %s" op op2) op_to_id in
      begin match op2_swap, op1_swap with
      | Some (op2_swap, _), Some (op1_swap, _) ->
        (* Luckily didn't happen *)
        failwith (Printf.sprintf "I have two candidates: %s,%s and %s,%s" op2_swap op1_swap op1_swap op2_swap)
      | Some (op2_swap, _), None ->
        failwith (Printf.sprintf "Found swap: %s,%s add to swaps to continue\n" op2 op2_swap)
      | None, Some (op1_swap, _) ->
        failwith (Printf.sprintf "Found swap: %s,%s add to swaps to continue\n" op1 op1_swap)
      | None, None ->
        (* Luckily didn't happen *)
        failwith (Printf.sprintf "Couldn't find any reparation for %s %s %s" op1 op op2)
      end
    end
  in
  let _ = List.fold_right (fun tree last_carry ->
    let name = name_of tree in
    let ind = String.sub name 1 (String.length name - 1) in
    if int_of_string ind = List.length trees - 1 then "DONE" else
    let xn = Printf.sprintf "x%s" ind in
    let yn = Printf.sprintf "y%s" ind in
    (* foo and bar always exist because the input can't be wired wrong.
       foo and bar themselves may be wrong though! *)
    let foo_id = Hashtbl.find op_to_id (Printf.sprintf "%s ^ %s" xn yn) in
    let bar_id = Hashtbl.find op_to_id (Printf.sprintf "%s & %s" xn yn) in
    if name = "z00" then begin
      (* foo00 is the output and bar00 is the carry *)
      if foo_id <> name then begin
        Printf.printf "z00,%s\n" foo_id;
      end;
      bar_id
    end else
    let baz_id = find_output_with_reparation last_carry "&" foo_id in
    let carry_id = find_output_with_reparation baz_id "|" bar_id in
    let _output_id = find_output_with_reparation last_carry "^" foo_id in
    carry_id
  ) trees "N/A" in
  let swapped = Hashtbl.fold (fun k _ acc -> k :: acc) swaps [] in
  Printf.printf "%s\n" (String.concat "," (List.sort String.compare swapped))
