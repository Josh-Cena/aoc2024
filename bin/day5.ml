let ( >> ) f g x = f (g x)
let num_to_ind lst = List.mapi (fun i x -> (x, i)) lst

let order_holds num_to_ind order =
  match
    ( List.assoc_opt (fst order) num_to_ind,
      List.assoc_opt (snd order) num_to_ind )
  with
  | Some i1, Some i2 -> i1 < i2
  | _ -> true

let solve1 data =
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
      (List.map int_of_string >> String.split_on_char ',')
      (List.tl (List.drop_while (fun x -> String.contains x '|') data))
  in
  let valid_lists =
    List.filter
      (fun lst -> List.for_all (order_holds (num_to_ind lst)) orders)
      lists
  in
  let middle_element lst =
    let len = List.length lst in
    List.nth lst (len / 2)
  in
  Printf.printf "%d\n"
    (List.fold_left ( + ) 0 (List.map middle_element valid_lists))

(* Yeah, very dumb *)
let rec sort_list_repeatedly orders lst =
  let rec sort_list orders lst =
    match orders with
    | [] -> (false, lst)
    | (a, b) :: rest ->
        let ind_a = List.find_index (fun x -> x = a) lst in
        let ind_b = List.find_index (fun x -> x = b) lst in
        begin match (ind_a, ind_b) with
        | Some i, Some j when i > j ->
            let lst' =
              List.mapi
                (fun k x -> if k = i then b else if k = j then a else x)
                lst
            in
            let _, lst'' = sort_list rest lst' in
            (true, lst'')
        | _ -> sort_list rest lst
        end
  in
  let has_changed, lst' = sort_list orders lst in
  if has_changed then sort_list_repeatedly orders lst' else lst'

let solve2 data =
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
      (List.map int_of_string >> String.split_on_char ',')
      (List.tl (List.drop_while (fun x -> String.contains x '|') data))
  in
  let invalid_lists =
    List.filter
      (fun lst -> not (List.for_all (order_holds (num_to_ind lst)) orders))
      lists
  in
  let sorted_lists = List.map (sort_list_repeatedly orders) invalid_lists in
  let middle_element lst =
    let len = List.length lst in
    List.nth lst (len / 2)
  in
  Printf.printf "%d\n"
    (List.fold_left ( + ) 0 (List.map middle_element sorted_lists))
