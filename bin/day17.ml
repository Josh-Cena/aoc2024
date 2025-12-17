let get_reg line =
  let prefix = "Register A: " in
  let prefix_len = String.length prefix in
  int_of_string (String.sub line prefix_len (String.length line - prefix_len))
let get_prog line =
  let prefix = "Program: " in
  let prefix_len = String.length prefix in
  String.sub line prefix_len (String.length line - prefix_len)
  |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list

let exec reg1_i reg2_i reg3_i prog =
  let reg1 = ref reg1_i in
  let reg2 = ref reg2_i in
  let reg3 = ref reg3_i in
  let pc = ref 0 in
  let get_combo op = match op with
    | 0 | 1 | 2 | 3 -> op
    | 4 -> !reg1
    | 5 -> !reg2
    | 6 -> !reg3
    | _ -> failwith "Invalid operation" in
  let output = ref [] in
  let step () =
    if !pc >= Array.length prog then false
    else let op = prog.(!pc + 1) in begin match prog.(!pc) with
    | 0 -> (* adv *) reg1 := !reg1 asr get_combo op; pc := !pc + 2;
    | 1 -> (* bxl *) reg2 := !reg2 lxor op; pc := !pc + 2;
    | 2 -> (* bst *) reg2 := get_combo op land 0b111; pc := !pc + 2;
    | 3 -> (* jnz *) if !reg1 <> 0 then pc := op else pc := !pc + 2;
    | 4 -> (* bxc *) reg2 := !reg2 lxor !reg3; pc := !pc + 2;
    | 5 -> (* out *) output := (get_combo op mod 8) :: !output; pc := !pc + 2;
    | 6 -> (* bdv *) reg2 := !reg1 asr get_combo op; pc := !pc + 2;
    | 7 -> (* cdv *) reg3 := !reg1 asr get_combo op; pc := !pc + 2;
    | _ -> failwith "Invalid instruction"
    end; true in
  while step () do () done;
  !output |> List.rev

let solve1 data =
  let (reg1_i, reg2_i, reg3_i, prog) = begin match data with
    | [l1; l2; l3; ""; l4] -> (get_reg l1, get_reg l2, get_reg l3, get_prog l4)
    | _ -> failwith "Invalid input"
  end in
  let output = exec reg1_i reg2_i reg3_i prog in
  Printf.printf "%s\n" (output |> List.map string_of_int |> String.concat ",")


(* For a given octet in a, what is the corresponding output? *)
type bit_info =
  (* A bit higher than this octet. The int is the offset from the last known bit
  (we determine a from the highest bit); true = not inverted.  *)
  (* Ex: (true, 2) => the output bit corresponds to the 3rd lowest known bit *)
  | Symbol of bool * int
  | Bit of int (* 1 or 0 *)

let shift_by v a0 a1 a2 =
  if v = 0 then [Bit a2; Bit a1; Bit a0]
  else if v = 1 then [Symbol (true, 0); Bit a2; Bit a1]
  else if v = 2 then [Symbol (true, 1); Symbol (true, 0); Bit a2]
  else if v = 3 then [Symbol (true, 2); Symbol (true, 1); Symbol (true, 0)]
  else if v = 4 then [Symbol (true, 3); Symbol (true, 2); Symbol (true, 1)]
  else if v = 5 then [Symbol (true, 4); Symbol (true, 3); Symbol (true, 2)]
  else if v = 6 then [Symbol (true, 5); Symbol (true, 4); Symbol (true, 3)]
  else if v = 7 then [Symbol (true, 6); Symbol (true, 5); Symbol (true, 4)]
  else failwith "Invalid shift value"

let xor_bit a_bit b_bit =
  match (a_bit, b_bit) with
  | (Bit a, Bit b) -> Bit (a lxor b)
  | (Symbol (st, name), Bit 1) -> Symbol (not st, name)
  | (Symbol (st, name), Bit 0) -> Symbol (st, name)
  | (Bit 1, Symbol (st, name)) -> Symbol (not st, name)
  | (Bit 0, Symbol (st, name)) -> Symbol (st, name)
  | _ -> failwith "Invalid xor"

let xor_bits bits1 bits2 =
  List.map2 xor_bit bits1 bits2

let is_bit_compatible existing num info =
  match info with
  | Bit i -> num = i
  | Symbol (sign, ind) ->
    let existing_bit =
      if ind >= Array.length existing then 0 else existing.(ind) in
    num = if sign then existing_bit else 1 - existing_bit

let split_bits num =
  let a0 = num land 0b001 in
  let a1 = (num land 0b010) lsr 1 in
  let a2 = (num land 0b100) lsr 2 in
  (a0, a1, a2)

(* The result grows from lowest bit to highest bit *)
let rec search_sol result info prog =
  match prog with
  | [] -> Some result
  | out :: rest ->
    let out0, out1, out2 = split_bits out in
    let res = List.mapi (fun i l -> (i, l)) info |> List.filter (fun (_, inf) ->
      List.for_all2 (is_bit_compatible result) [out2; out1; out0] inf
    ) in
    List.find_map (fun (i, _) ->
      let res0, res1, res2 = split_bits i in
      let new_result = Array.append (Array.of_list [res0; res1; res2]) result in
      search_sol new_result info rest
    ) res

let solve2 data =
  let (_, _, _, prog) = begin match data with
    | [l1; l2; l3; ""; l4] -> (get_reg l1, get_reg l2, get_reg l3, get_prog l4)
    | _ -> failwith "Invalid input"
  end in
  (* Only two personalized parts of the input *)
  let x = prog.(3) in
  let y = prog.(7) in
  let x0, x1, x2 = split_bits x in
  let y0, y1, y2 = split_bits y in
  assert (prog = Array.of_list [2;4;1;x;7;5;1;y;4;4;5;5;0;3;3;0]);
  let info = List.map (fun i ->
    let a0, a1, a2 = split_bits i in
    (shift_by (i lxor x) a0 a1 a2)
    |> xor_bits [Bit a2; Bit a1; Bit a0]
    |> xor_bits [Bit x2; Bit x1; Bit x0]
    |> xor_bits [Bit y2; Bit y1; Bit y0])
    [0;1;2;3;4;5;6;7] in
  match search_sol (Array.make 0 0) info (List.rev (Array.to_list prog)) with
  | None -> Printf.printf "No solution found\n"
  | Some res ->
    Printf.printf "%d\n" (int_of_string (Printf.sprintf "0b%s" (res |> Array.to_list |> List.rev |> List.map string_of_int |> String.concat "")));
