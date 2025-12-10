let solve1 data =
  let line = List.hd data in
  let chunks = Array.init (9 * String.length line) (fun _ -> -1) in
  let i = ref 0 in
  let id = ref 0 in
  String.iteri (fun ind c ->
    let digit = Char.code c - Char.code '0' in
    if ind mod 2 = 0 then
      (for j = 0 to digit - 1 do
        chunks.(!i + j) <- !id
      done; 
      id := !id + 1);
    i := !i + digit;
  ) line;
  let blanks = ref (Array.to_list chunks
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (_, x) -> x = -1)
    |> List.map fst) in
  let exception Exit in
    try for i = Array.length chunks - 1 downto 0 do
      if List.hd !blanks >= i then
        raise Exit
      else if chunks.(i) <> -1 then
        let pos = List.hd !blanks in
        blanks := List.tl !blanks;
        chunks.(pos) <- chunks.(i);
        chunks.(i) <- -1
    done; with Exit -> ();
  let result = Array.mapi (fun i x -> (i, x)) chunks
    |> Array.to_list
    |> List.filter (fun (_, x) -> x <> -1)
    |> List.map (fun (i, x) -> i * x)
    |> List.fold_left (+) 0 in
  Printf.printf "%d\n" result

type chunk = { id: int; occ: int; ind: int }
type seg = { chunks: chunk list; total_size: int; free_size: int; ind: int }

let solve2 data =
  let line = List.hd data in
  let chunks = String.to_seq line |> List.of_seq |> List.map (fun c -> Char.code c - Char.code '0') in
  let id = ref 0 in
  let build_segments chunks =
    let rec aux chunks acc ind =
      match chunks with
      | occ :: [] ->
        let chunks = [{ id = !id; occ; ind }] in
        id := !id + 1;
        let seg = { chunks = chunks; total_size = occ; free_size = 0; ind } in
        List.rev (seg :: acc)
      | occ :: free :: t ->
        let chunks = [{ id = !id; occ; ind }] in
        id := !id + 1;
        let seg = { chunks = chunks; total_size = occ + free; free_size = free; ind } in
        aux t (seg :: acc) (ind + occ + free)
      | _ -> List.rev acc
    in
    aux chunks [] 0
  in
  let segments = ref (Array.of_list (build_segments chunks)) in
  for i = Array.length !segments - 1 downto 0 do
    let seg = (!segments).(i) in
    let rec move_all_chunks chunks =
      match chunks with
      | [] -> ()
      | { id; occ; _ } :: t ->
        let pos = Array.find_index (fun s -> s.free_size >= occ) !segments in
        begin match pos with
        | Some p when p < i ->
          let target_seg = (!segments).(p) in
          let new_ind = match target_seg.chunks with
            | [] -> target_seg.ind
            | hd :: _ -> hd.ind + hd.occ in
          (* Update target segment *)
          let new_target_chunks = { id; occ; ind = new_ind } :: target_seg.chunks in
          let new_target_free = target_seg.free_size - occ in
          (!segments).(p) <- { target_seg with chunks = new_target_chunks; free_size = new_target_free };
          (* Update source segment *)
          let new_source_chunks = List.filter (fun c -> c.id <> id) seg.chunks in
          let new_source_free = seg.free_size + occ in
          (!segments).(i) <- { seg with chunks = new_source_chunks; free_size = new_source_free };
        | _ -> ()
        end;
        move_all_chunks t
    in
    move_all_chunks seg.chunks
  done;
  let checksum_of_chunk chunk =
    List.fold_left (+) 0 (List.init chunk.occ (fun i -> (chunk.ind + i) * chunk.id)) in
  let checksum_of_segment seg =
    List.map checksum_of_chunk seg.chunks
    |> List.fold_left (+) 0 in
  let result = Array.map checksum_of_segment !segments
    |> Array.fold_left (+) 0 in
  Printf.printf "%d\n" result
