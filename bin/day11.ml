let counts list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
        let count =
          List.fold_left (fun c y -> if y = x then c + 1 else c) 0 xs
        in
        aux ((x, count + 1) :: acc) (List.filter (fun y -> y <> x) xs)
  in
  aux [] list

let solve n data =
  let line = List.hd data in
  let nums = counts (List.map int_of_string (String.split_on_char ' ' line)) in
  let rec evolve n nums =
    if n = 0 then nums
    else
      let new_nums =
        List.fold_left
          (fun acc (x, count) ->
            let sx = string_of_int x in
            let new_x =
              if x = 0 then [ 1 ]
              else if String.length sx mod 2 = 0 then
                [
                  int_of_string (String.sub sx 0 (String.length sx / 2));
                  int_of_string
                    (String.sub sx
                       (String.length sx / 2)
                       (String.length sx / 2));
                ]
              else [ x * 2024 ]
            in
            List.fold_left
              (fun acc' nx ->
                match List.assoc_opt nx acc' with
                | Some c -> (nx, c + count) :: List.remove_assoc nx acc'
                | None -> (nx, count) :: acc')
              acc new_x)
          [] nums
      in
      evolve (n - 1) new_nums
  in
  let final_nums = evolve n nums in
  Printf.printf "%d\n"
    (List.fold_left (fun acc (_, count) -> acc + count) 0 final_nums)

let solve1 = solve 25
let solve2 = solve 75
