open Q

type mat22 = { a : Q.t; b : Q.t; c : Q.t; d : Q.t }

let det { a; b; c; d } = (a * d) - (b * c)

let inv m =
  let d = det m in
  if d = Q.zero then None
  else Some { a = m.d / d; b = -m.b / d; c = -m.c / d; d = m.a / d }

let btn_re = Re.Perl.compile_pat {|Button [AB]: X\+(\d+), Y\+(\d+)|}
let prize_re = Re.Perl.compile_pat {|Prize: X=(\d+), Y=(\d+)|}

let rec parse_lines lines prize_shift =
  match lines with
  | l1 :: l2 :: l3 :: rest ->
      let m1 = Re.exec btn_re l1 in
      let m2 = Re.exec btn_re l2 in
      let m3 = Re.exec prize_re l3 in
      let mat =
        {
          a = of_string (Re.Group.get m1 1);
          b = of_string (Re.Group.get m2 1);
          c = of_string (Re.Group.get m1 2);
          d = of_string (Re.Group.get m2 2);
        }
      in
      let prize =
        ( of_string (Re.Group.get m3 1) + prize_shift,
          of_string (Re.Group.get m3 2) + prize_shift )
      in
      (mat, prize)
      :: begin match rest with
      | [] -> []
      | "" :: tail -> parse_lines tail prize_shift
      | _ -> failwith "Expected empty line between blocks"
      end
  | _ -> []

(* Solve: mat * X = prize *)
let solve_lp mat (prize_x, prize_y) =
  match inv mat with
  | None ->
      if mat.a / mat.c = prize_x / prize_y then (
        (* There's a tradeoff between the two buttons. a*nA + b*nB = prize_x *)
        (* We want to minimize cost = 3*nA + nB *)
        (* cost = prize_x / b + (3b - a) / b * nA *)
        Printf.printf "LP problem: %s*nA + %s*nB = %s\n" (to_string mat.a)
          (to_string mat.b) (to_string prize_x);
        failwith "Unimplemented")
      else None
  | Some inv_mat ->
      let x = (inv_mat.a * prize_x) + (inv_mat.b * prize_y) in
      let y = (inv_mat.c * prize_x) + (inv_mat.d * prize_y) in
      if
        x >= of_int 0
        && y >= of_int 0
        && Z.equal x.den (Z.of_int 1)
        && Z.equal y.den (Z.of_int 1)
      then Some (x, y)
      else None

let solve prize_shift data =
  let blocks = parse_lines data (of_int prize_shift) in
  let solutions =
    List.filter_map (fun (mat, prize) -> solve_lp mat prize) blocks
  in
  let total =
    List.map (fun (x, y) -> (of_int 3 * x) + y) solutions
    |> List.fold_left ( + ) (of_int 0)
  in
  Printf.printf "%s\n" (to_string total)

let solve1 = solve 0
let solve2 = solve 10000000000000
