let line_re = Re.Perl.compile_pat {|p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)|}

let parse_line line =
  let m = Re.exec line_re line in
  let px = int_of_string (Re.Group.get m 1) in
  let py = int_of_string (Re.Group.get m 2) in
  let vx = int_of_string (Re.Group.get m 3) in
  let vy = int_of_string (Re.Group.get m 4) in
  ((px, py), (vx, vy))

let pos_mod x y =
  let r = x mod y in
  if r < 0 then r + y else r

let move_robot w h ((px, py), (vx, vy)) =
  ((pos_mod (px + vx) w, pos_mod (py + vy) h), (vx, vy))

let quadrant w h x y =
  let half_w = (w - 1) / 2 in
  let half_h = (h - 1) / 2 in
  if x = half_w || y = half_h then -1
  else if x < half_w && y < half_h then 0
  else if x > half_w && y < half_h then 1
  else if x < half_w && y > half_h then 2
  else 3

let solve1 data =
  let first_line = List.hd data in
  let w, h =
    let parts = String.split_on_char ' ' first_line in
    (int_of_string (List.nth parts 0), int_of_string (List.nth parts 1))
  in
  let robots = List.map parse_line (List.tl data) in
  let new_pos =
    List.fold_left
      (fun acc _ -> List.map (move_robot w h) acc)
      robots
      (List.init 100 (fun _ -> 0))
  in
  let quadrants = List.map (fun ((px, py), _) -> quadrant w h px py) new_pos in
  let counts = Array.make 4 0 in
  List.iter (fun q -> if q >= 0 then counts.(q) <- counts.(q) + 1) quadrants;
  Printf.printf "%d\n" (Array.fold_left ( * ) 1 counts)

let print_board w h robots =
  let board = Array.make_matrix h w '.' in
  List.iter (fun ((px, py), _) -> board.(py).(px) <- '#') robots;
  Array.iter
    (fun row -> Printf.printf "%s\n" (String.of_seq (Array.to_seq row)))
    board

let solve2 data =
  let first_line = List.hd data in
  let w, h =
    let parts = String.split_on_char ' ' first_line in
    (int_of_string (List.nth parts 0), int_of_string (List.nth parts 1))
  in
  let robots = List.map parse_line (List.tl data) in
  let _ =
    List.fold_left
      (fun acc step ->
        let new_pos = List.map (move_robot w h) acc in
        let pos_count = Hashtbl.create 100 in
        List.iter
          (fun ((px, py), _) ->
            let key = (px, py) in
            Hashtbl.replace pos_count key
              (1 + try Hashtbl.find pos_count key with Not_found -> 0))
          new_pos;
        (* Fact revealed to me in a dream (Because, obviously, the question is
      reverse-engineered from this starting point with no overlap)
      Even if this isn't true there are many other ways, such as searching for
      straight edges *)
        if Hashtbl.fold (fun _ count acc -> acc && count = 1) pos_count true
        then (
          Printf.printf "t=%d\n" (step + 1);
          print_board w h new_pos);
        new_pos)
      robots
      (List.init 10000 (fun i -> i))
  in
  ()
