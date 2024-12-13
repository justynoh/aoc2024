open Core

let info = Cmdliner.Cmd.info "aoc2024"

let term =
  let open Cmdliner.Term.Syntax in
  let+ task_opt =
    let open Cmdliner.Arg in
    value & pos 0 (some string) None & info [] ~docv:"TASK"
  and+ filepath_opt =
    let open Cmdliner.Arg in
    value & pos 1 (some string) None & info [] ~docv:"FILEPATH"
  in
  (match filepath_opt with
  | None -> "Please specify an input file path."
  | Some filepath -> (
      let input = Core.In_channel.read_all filepath in
      match task_opt with
      | Some "1a" -> Aoc2024.Task1.a input
      | Some "1b" -> Aoc2024.Task1.b input
      | Some "2a" -> Aoc2024.Task2.a input
      | Some "2b" -> Aoc2024.Task2.b input
      | Some "3a" -> Aoc2024.Task3.a input
      | Some "3b" -> Aoc2024.Task3.b input
      | Some "4a" -> Aoc2024.Task4.a input
      | Some "4b" -> Aoc2024.Task4.b input
      | Some "5a" -> Aoc2024.Task5.a input
      | Some "5b" -> Aoc2024.Task5.b input
      | Some _ | None -> "No task specified."))
  |> print_endline

let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
