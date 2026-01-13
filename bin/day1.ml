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
  let counts2 =
    List.fold_left
      (fun acc n ->
        let count = try List.assoc n acc with Not_found -> 0 in
        (n, count + 1) :: List.remove_assoc n acc)
      [] nums2
  in
  let total =
    List.fold_left
      (fun acc n1 ->
        acc + ((try List.assoc n1 counts2 with Not_found -> 0) * n1))
      0 nums1
  in
  Printf.printf "%d\n" total
