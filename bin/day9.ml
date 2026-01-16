let solve1 data =
  let line = List.hd data in
  let blocks = Array.init (9 * String.length line) (fun _ -> -1) in
  let i = ref 0 in
  let id = ref 0 in
  String.iteri
    (fun ind c ->
      let digit = Char.code c - Char.code '0' in
      if ind mod 2 = 0 then begin
        for j = 0 to digit - 1 do
          blocks.(!i + j) <- !id
        done;
        id := !id + 1
      end;
      i := !i + digit)
    line;
  let blanks =
    ref
      (Array.to_list blocks
      |> List.mapi (fun i x -> (i, x))
      |> List.filter_map (fun (i, x) -> if x = -1 then Some i else None))
  in
  let rec loop i =
    if i < 0 || List.hd !blanks >= i then ()
    else begin
      if blocks.(i) <> -1 then begin
        let pos = List.hd !blanks in
        blanks := List.tl !blanks;
        blocks.(pos) <- blocks.(i);
        blocks.(i) <- -1
      end;
      loop (i - 1)
    end
  in
  loop (Array.length blocks - 1);
  let result =
    Array.to_list blocks
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (_, x) -> x <> -1)
    |> List.map (fun (i, x) -> i * x)
    |> List.fold_left ( + ) 0
  in
  Printf.printf "%d\n" result

type file = { id : int; occ : int; ind : int }
type seg = { files : file list; free_size : int; ind : int }

let solve2 data =
  let line = List.hd data in
  let blocks =
    String.to_seq line |> List.of_seq
    |> List.map (fun c -> Char.code c - Char.code '0')
  in
  let build_segments blocks =
    let rec aux (id, blocks) acc ind =
      match blocks with
      | occ :: [] ->
          let files = [ { id; occ; ind } ] in
          let seg = { files; free_size = 0; ind } in
          List.rev (seg :: acc)
      | occ :: free :: t ->
          let files = [ { id; occ; ind } ] in
          let seg = { files; free_size = free; ind } in
          aux (id + 1, t) (seg :: acc) (ind + occ + free)
      | _ -> List.rev acc
    in
    aux (0, blocks) [] 0
  in
  let segments = Array.of_list (build_segments blocks) in
  let rec move_all_files i remaining_files = function
    | [] -> remaining_files
    | { id; occ; ind } :: t ->
        let pos = Array.find_index (fun s -> s.free_size >= occ) segments in
        begin match pos with
        | Some p when p < i ->
            let target_seg = segments.(p) in
            let new_ind =
              match target_seg.files with
              | [] -> target_seg.ind
              | hd :: _ -> hd.ind + hd.occ
            in
            segments.(p) <-
              {
                target_seg with
                files = { id; occ; ind = new_ind } :: target_seg.files;
                free_size = target_seg.free_size - occ;
              };
            move_all_files i remaining_files t
        | _ -> move_all_files i ({ id; occ; ind } :: remaining_files) t
        end
  in
  for i = Array.length segments - 1 downto 0 do
    segments.(i) <-
      { (segments.(i)) with files = move_all_files i [] segments.(i).files }
  done;
  let checksum_of_file file =
    List.fold_left ( + ) 0
      (List.init file.occ (fun i -> (file.ind + i) * file.id))
  in
  let checksum_of_segment seg =
    List.map checksum_of_file seg.files |> List.fold_left ( + ) 0
  in
  let result =
    Array.map checksum_of_segment segments |> Array.fold_left ( + ) 0
  in
  Printf.printf "%d\n" result
