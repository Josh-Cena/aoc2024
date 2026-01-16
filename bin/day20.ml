let num_working_cheats distances i j radius =
  let count = ref 0 in
  let h = Array.length distances in
  let w = Array.length distances.(0) in
  for ni = i - radius to i + radius do
    for nj = j - radius + abs (i - ni) to j + radius - abs (i - ni) do
      if ni < 0 || ni >= h || nj < 0 || nj >= w then ()
      else if
        distances.(ni).(nj) - distances.(i).(j)
        >= 100 + abs (i - ni) + abs (j - nj)
      then count := !count + 1
    done
  done;
  !count

let parse_input data =
  let mat =
    Array.of_list
      (List.map (fun line -> Array.of_seq (String.to_seq line)) data)
  in
  let start_pos =
    mat
    |> Array.find_mapi (fun i ->
        Array.find_mapi (fun j c -> if c = 'S' then Some (i, j) else None))
    |> Option.get
  in
  let distances =
    Array.make_matrix (Array.length mat) (Array.length mat.(0)) (-1)
  in
  let rec loop cur path distance =
    let i, j = cur in
    distances.(i).(j) <- distance;
    if mat.(i).(j) = 'E' then path
    else
      let new_pos =
        List.find
          (fun (ni, nj) ->
            ni >= 0
            && ni < Array.length mat
            && nj >= 0
            && nj < Array.length mat.(0)
            && distances.(ni).(nj) = -1
            && mat.(ni).(nj) <> '#')
          [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
      in
      loop new_pos (cur :: path) (distance + 1)
  in
  let path = loop start_pos [] 0 in
  (distances, List.rev path)

let solve radius data =
  let distances, path = parse_input data in
  let cheats =
    List.map (fun (i, j) -> num_working_cheats distances i j radius) path
  in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 cheats)

let solve1 = solve 2
let solve2 = solve 20
