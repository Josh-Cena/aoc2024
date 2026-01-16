let parse_input data =
  let orders =
    List.map
      (fun x ->
        match String.split_on_char '|' x with
        | [ a; b ] -> (int_of_string a, int_of_string b)
        | _ -> failwith "Invalid order format")
      (List.take_while (fun x -> String.contains x '|') data)
  in
  let lists =
    List.map
      (fun x -> String.split_on_char ',' x |> List.map int_of_string)
      (List.tl (List.drop_while (fun x -> String.contains x '|') data))
  in
  (orders, lists)

module IntMap = Map.Make (Int)

let map_num_to_ind lst =
  List.fold_right
    (fun (i, x) -> IntMap.add x i)
    (List.mapi (fun i x -> (i, x)) lst)
    IntMap.empty

let order_holds num_to_ind (a, b) =
  match (IntMap.find_opt a num_to_ind, IntMap.find_opt b num_to_ind) with
  | Some i1, Some i2 -> i1 < i2
  | _ -> true

let solve1 data =
  let orders, lists = parse_input data in
  let valid_lists =
    List.filter
      (fun lst -> List.for_all (order_holds (map_num_to_ind lst)) orders)
      lists
  in
  let middle_element lst = List.nth lst (List.length lst / 2) in
  Printf.printf "%d\n"
    (List.fold_left ( + ) 0 (List.map middle_element valid_lists))

(* Yeah, very dumb *)
let rec sort_list_repeatedly orders num_to_ind =
  let rec sort_list orders num_to_ind =
    match orders with
    | [] -> (false, num_to_ind)
    | (a, b) :: rest ->
        let ind_a = IntMap.find_opt a num_to_ind in
        let ind_b = IntMap.find_opt b num_to_ind in
        begin match (ind_a, ind_b) with
        | Some i, Some j when i > j ->
            let _, num_to_ind' =
              num_to_ind |> IntMap.add a j |> IntMap.add b i |> sort_list rest
            in
            (true, num_to_ind')
        | _ -> sort_list rest num_to_ind
        end
  in
  let has_changed, num_to_ind' = sort_list orders num_to_ind in
  if has_changed then sort_list_repeatedly orders num_to_ind' else num_to_ind

let solve2 data =
  let orders, lists = parse_input data in
  let invalid_lists =
    List.filter
      (fun lst -> not (List.for_all (order_holds (map_num_to_ind lst)) orders))
      lists
  in
  let sorted_lists =
    List.map
      (fun lst -> sort_list_repeatedly orders (map_num_to_ind lst))
      invalid_lists
  in
  let middle_element num_to_ind =
    let mapping = IntMap.bindings num_to_ind in
    List.find (fun (_, ind) -> ind = List.length mapping / 2) mapping |> fst
  in
  Printf.printf "%d\n"
    (List.fold_left ( + ) 0 (List.map middle_element sorted_lists))
