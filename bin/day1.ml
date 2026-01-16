let solve1 data =
  let nums1, nums2 =
    List.split
      (List.map
         (fun x ->
           match Str.split (Str.regexp " +") x with
           | [ a; b ] -> (int_of_string a, int_of_string b)
           | _ -> failwith "Invalid input")
         data)
  in
  let nums1' = List.sort compare nums1 in
  let nums2' = List.sort compare nums2 in
  let differences = List.map2 (fun a b -> abs (b - a)) nums1' nums2' in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 differences)

module IntMap = Map.Make (Int)

let counts lst =
  List.fold_left
    (fun acc x ->
      IntMap.update x (function None -> Some 1 | Some n -> Some (n + 1)) acc)
    IntMap.empty lst

let solve2 data =
  let nums1, nums2 =
    List.split
      (List.map
         (fun x ->
           match Str.split (Str.regexp " +") x with
           | [ a; b ] -> (int_of_string a, int_of_string b)
           | _ -> failwith "Invalid input")
         data)
  in
  let counts1 = counts nums1 in
  let counts2 = counts nums2 in
  let join_counts =
    IntMap.merge
      (fun _ c1 c2 ->
        match (c1, c2) with Some n1, Some n2 -> Some (n1 * n2) | _ -> None)
      counts1 counts2
  in
  Printf.printf "%d\n"
    (IntMap.fold (fun k v acc -> acc + (k * v)) join_counts 0)
