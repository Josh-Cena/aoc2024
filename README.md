# Advent of Code 2022

- Language: ![OCaml](https://img.shields.io/badge/OCaml-%23E98407.svg?style=for-the-badge&logo=ocaml&logoColor=white) (5.2.1)
- Package Manager: [opam](https://opam.ocaml.org/)
- [Writeup](https://joshcena.com/notes/aoc/)

This repo uses my standard AoC setup. Inputs are stored as `inputs/day{n}/{name}.txt`. By default `name` is `real` (the real question). To run a specific day's solution, use the following command:

```bash
opam exec -- dune exec aoc2024 {day} {part} {name}
```

For example, to run the solution for day 1, part 2 with the example input:

```bash
opam exec -- dune exec aoc2024 1 2 ex
```

(And make sure that `inputs/day1/ex.txt` exists.)
