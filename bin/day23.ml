let solve1 data =
  let nodes = Hashtbl.create (List.length data) in
  let edges = Hashtbl.create (List.length data) in
  let add_line l =
    let parts = String.split_on_char '-' l in
    let u = List.nth parts 0 in
    let v = List.nth parts 1 in
    Hashtbl.add edges (u, v) ();
    Hashtbl.add edges (v, u) ();
    Hashtbl.replace nodes u ();
    Hashtbl.replace nodes v ()
  in
  List.iter add_line data;
  let triangles = Hashtbl.create (List.length data) in
  Hashtbl.iter (fun (u, v) _ ->
    Hashtbl.iter (fun k _ ->
      if String.starts_with ~prefix:"t" k && Hashtbl.mem edges (u, k) && Hashtbl.mem edges (v, k) then
        let key = match List.sort compare [u; v; k] with
          | [a; b; c] -> (a, b, c)
          | _ -> failwith "Unexpected case"
        in
        Hashtbl.replace triangles key ()
    ) nodes
  ) edges;
  Printf.printf "%d\n" (Hashtbl.length triangles)

let solve2 _ = Printf.printf "Life's too short; please see day23.py\n"
