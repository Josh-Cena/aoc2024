let parse_input data =
  let chunks = Str.split (Str.regexp "\n\n") (String.concat "\n" data) in
  let values =
    List.nth chunks 0 |> String.split_on_char '\n'
    |> List.map (Str.split (Str.regexp ": "))
    |> List.map (fun x -> (List.nth x 0, int_of_string (List.nth x 1) == 1))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let gates =
    List.nth chunks 1 |> String.split_on_char '\n'
    |> List.map (Str.split (Str.regexp " -> "))
    |> List.map (fun x ->
        let parts = Str.split (Str.regexp " ") (List.nth x 0) in
        let name = List.nth x 1 in
        match parts with
        | [ x; y; z ] -> (name, (x, y, z))
        | _ -> failwith "Invalid input")
    |> List.to_seq |> Hashtbl.of_seq
  in
  (values, gates)

let rec eval_gate values gates name =
  if Hashtbl.mem values name then Hashtbl.find values name
  else
    let x, op, y = Hashtbl.find gates name in
    let x_val = eval_gate values gates x in
    let y_val = eval_gate values gates y in
    let result =
      match op with
      | "AND" -> x_val && y_val
      | "OR" -> x_val || y_val
      | "XOR" -> x_val <> y_val
      | _ -> failwith "Invalid operator"
    in
    Hashtbl.add values name result;
    result

let solve1 data =
  let values, gates = parse_input data in
  let output_gates =
    Hashtbl.fold
      (fun name _ acc ->
        if String.starts_with ~prefix:"z" name then name :: acc else acc)
      gates []
  in
  let outputs =
    List.map (eval_gate values gates)
      (List.sort (Fun.flip String.compare) output_gates)
  in
  Printf.printf "%d\n"
    (List.fold_left (fun acc x -> (acc * 2) + if x then 1 else 0) 0 outputs)

module GateSet = Set.Make (String)

(* For each adder zn, it receives, in order: carry(n), xn, yn.
   The graph is structured such that
   - foo = xn ^ yn
   - zn = carry(n) ^ foo
   - bar = xn & yn
   - baz = carry(n) & foo
   - carry(n+1) = baz | bar
   We just need to check if the gate matches this structure.
*)
let is_expected_gate k x op y carry_or_foo baz_or_bar =
  (* First does not receive carry *)
  (op = "XOR" && (x = "x00" || x = "y00") && k = "z00")
  || (op = "AND" && (x = "x00" || x = "y00") && GateSet.mem k carry_or_foo)
  || op = "XOR"
     && (String.starts_with ~prefix:"x" x || String.starts_with ~prefix:"y" x)
     && GateSet.mem k carry_or_foo
  || op = "XOR" && GateSet.mem x carry_or_foo && GateSet.mem y carry_or_foo
     && String.starts_with ~prefix:"z" k
  || op = "AND"
     && (String.starts_with ~prefix:"x" x || String.starts_with ~prefix:"y" x)
     && GateSet.mem k baz_or_bar
  || op = "AND" && GateSet.mem x carry_or_foo && GateSet.mem y carry_or_foo
     && GateSet.mem k baz_or_bar
  || op = "OR" && GateSet.mem x baz_or_bar && GateSet.mem y baz_or_bar
     (* Last does not have carry[44]; instead z45 *)
     && (GateSet.mem k carry_or_foo || k = "z45")

let solve2 data =
  let _, gates = parse_input data in
  let carry_or_foo, baz_or_bar =
    Hashtbl.fold
      (fun _ (x, op, y) (carry_or_foo, baz_or_bar) ->
        if
          (op = "XOR" || op = "AND")
          && (not (String.starts_with ~prefix:"x" x))
          && not (String.starts_with ~prefix:"y" x)
        then (carry_or_foo |> GateSet.add x |> GateSet.add y, baz_or_bar)
        else if op = "OR" then
          (carry_or_foo, baz_or_bar |> GateSet.add x |> GateSet.add y)
        else (carry_or_foo, baz_or_bar))
      gates
      (GateSet.empty, GateSet.empty)
  in
  (* There could be false negatives here, such as if foo10 is swapped with foo11,
     but looks like it didn't happen *)
  let suspicious =
    Hashtbl.fold
      (fun k (x, op, y) acc ->
        if not (is_expected_gate k x op y carry_or_foo baz_or_bar) then
          (k, x, op, y) :: acc
        else acc)
      gates []
  in
  assert (List.length suspicious = 8);
  Printf.printf "%s\n"
    (List.map (fun (k, _, _, _) -> k) suspicious
    |> List.sort String.compare |> String.concat ",")
