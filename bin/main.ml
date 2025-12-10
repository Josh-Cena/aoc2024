let lines_of_file filename =
  let ic = open_in filename in
  let rec aux acc =
    try
      let line = input_line ic in
      aux (line :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  aux []
let day = Sys.argv.(1)
let prob = Sys.argv.(2)
let input = if Array.length Sys.argv > 3 then Sys.argv.(3) else "real"
let filename = Printf.sprintf "inputs/day%s/%s.txt" day input

let contents = lines_of_file filename |> List.drop_while ((=) "") |> List.rev
let () = match (day, prob) with
  | ("1", "1") -> Day1.solve1 contents
  | ("1", "2") -> Day1.solve2 contents
  | ("2", "1") -> Day2.solve1 contents
  | ("2", "2") -> Day2.solve2 contents
  | ("3", "1") -> Day3.solve1 contents
  | ("3", "2") -> Day3.solve2 contents
  | ("4", "1") -> Day4.solve1 contents
  | ("4", "2") -> Day4.solve2 contents
  | ("5", "1") -> Day5.solve1 contents
  | ("5", "2") -> Day5.solve2 contents
  | ("6", "1") -> Day6.solve1 contents
  | ("6", "2") -> Day6.solve2 contents
  | ("7", "1") -> Day7.solve1 contents
  | ("7", "2") -> Day7.solve2 contents
  | ("8", "1") -> Day8.solve1 contents
  | ("8", "2") -> Day8.solve2 contents
  | ("9", "1") -> Day9.solve1 contents
  | ("9", "2") -> Day9.solve2 contents
  | ("10", "1") -> Day10.solve1 contents
  | ("10", "2") -> Day10.solve2 contents
  | ("11", "1") -> Day11.solve1 contents
  | ("11", "2") -> Day11.solve2 contents
  | ("12", "1") -> Day12.solve1 contents
  | ("12", "2") -> Day12.solve2 contents
  | ("13", "1") -> Day13.solve1 contents
  | ("13", "2") -> Day13.solve2 contents
  | ("14", "1") -> Day14.solve1 contents
  | ("14", "2") -> Day14.solve2 contents
  | _ -> Printf.printf "Day %s Problem %s not implemented\n" day prob
