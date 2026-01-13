let solve1 data =
  let line = String.concat "" data in
  let re = Re.Perl.compile (Re.Perl.re {|mul\((\d{1,3}),(\d{1,3})\)|}) in
  let matches =
    Re.all re line |> List.map (fun g -> (Re.Group.get g 1, Re.Group.get g 2))
  in
  let products =
    List.map (fun (a, b) -> int_of_string a * int_of_string b) matches
  in
  let total = List.fold_left ( + ) 0 products in
  Printf.printf "%d\n" total

let solve2 data =
  let line = String.concat "" data in
  let re =
    Re.Perl.compile
      (Re.Perl.re {|mul\((\d{1,3}),(\d{1,3})\)|do\(\)|(don't)\(\)|})
  in
  let matches =
    Re.all re line
    |> List.map (fun g ->
        (Re.Group.get g 0, Re.Group.get_opt g 1, Re.Group.get_opt g 2))
  in
  let _, matches' =
    List.fold_left
      (fun (cur_st, muls) (s, a, b) ->
        match s with
        | "do()" -> (true, muls)
        | "don't()" -> (false, muls)
        | _ -> begin
            match (a, b) with
            | Some a, Some b ->
                if cur_st then
                  (cur_st, (int_of_string a, int_of_string b) :: muls)
                else (cur_st, muls)
            | _ -> (cur_st, muls)
          end)
      (true, []) matches
  in
  let products = List.map (fun (a, b) -> a * b) matches' in
  let total = List.fold_left ( + ) 0 products in
  Printf.printf "%d\n" total
