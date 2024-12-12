# AOC 2024

This repo contains my solutions to AOC 2024, written in OCaml.

## Setup

Ensure you have opam installed:

```bash
$ brew install opam
```

Then, `opam install` relevant dependencies:

- `ocaml`
- `ocamlformat`
- `dune`
- `ocaml-lsp-server`
- `core`

## Run the code

```bash
$ dune build
$ dune exec -- aoc2024 <TASK> <PATH_TO_INPUT_FILE>
```

`<TASK>` consists of the day and part (e.g. `1a`, `1b`, `2a`, `2b`, etc.). `<PATH_TO_INPUT_FILE>` is the string path to the input file to be used for this part, relative to the root directory.

When updating the code, run `dune fmt` to format the files.
