let (>>) f g x = f (g x)

let solve1 data =
  let orders = List.map (fun x -> match String.split_on_char '|' x with
    | [a; b] -> (int_of_string a, int_of_string b)
    | _ -> failwith "Invalid order format"
  ) (List.take_while (fun x -> String.contains x '|') data) in
  let lists = List.map (List.map int_of_string >> String.split_on_char ',') (List.tl (List.drop_while (fun x -> String.contains x '|') data)) in
  let num_to_ind lst = List.mapi (fun i x -> (x, i)) lst in
  let order_holds num_to_ind order =
    match List.assoc_opt (fst order) num_to_ind, List.assoc_opt (snd order) num_to_ind with
    | Some i1, Some i2 -> i1 < i2
    | _ -> true
  in
  let valid_lists = List.filter (fun lst -> List.for_all (order_holds (num_to_ind lst)) orders) lists in
  let middle_element lst = let len = List.length lst in List.nth lst (len / 2) in
  Printf.printf "%d\n" (List.fold_left (+) 0 (List.map middle_element valid_lists))

let solve2 _ = ()
