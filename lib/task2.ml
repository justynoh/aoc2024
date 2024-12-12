open Core

(* Parse input string *)
let parseInput input =
  input |> String.split_lines
  |> List.map ~f:(fun lines ->
         lines |> String.split ~on:' ' |> List.map ~f:Int.of_string)

(* Perform the task *)
let isRecordValid record =
  match record with
  | [] -> false
  | _ :: recordtail ->
      let zipped, _ = List.zip_with_remainder record recordtail in
      let diffs = List.map zipped ~f:(fun (x, y) -> x - y) in
      (List.for_all diffs ~f:(fun diff -> diff > 0)
      || List.for_all diffs ~f:(fun diff -> diff < 0))
      && List.for_all diffs ~f:(fun diff -> -3 <= diff && diff <= 3)

let rec sublistsRemovingOne list =
  match list with
  | [] -> []
  | x :: xs -> xs :: List.map (sublistsRemovingOne xs) ~f:(fun l -> x :: l)

let mainA input = input |> List.count ~f:isRecordValid

let mainB input =
  input
  |> List.count ~f:(fun input ->
         isRecordValid input
         || List.exists (sublistsRemovingOne input) ~f:(fun sublist ->
                isRecordValid sublist))

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> parseInput |> mainA |> stringifyOutput
let b input = input |> parseInput |> mainB |> stringifyOutput
