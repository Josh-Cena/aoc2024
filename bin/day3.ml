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
  let _, total =
    List.fold_left
      (fun (cur_st, res) (s, a, b) ->
        match s with
        | "do()" -> (true, res)
        | "don't()" -> (false, res)
        | _ -> begin
            match (a, b) with
            | Some a, Some b ->
                if cur_st then
                  (cur_st, (int_of_string a * int_of_string b) + res)
                else (cur_st, res)
            | _ -> (cur_st, res)
          end)
      (true, 0) matches
  in
  Printf.printf "%d\n" total
