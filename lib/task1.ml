open Core

(* Parse input string *)
let parseInput input =
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         let values =
           line |> String.split ~on:' '
           |> List.filter ~f:(fun v -> String.(v <> ""))
         in
         match values with
         | [ x; y ] -> (Int.of_string x, Int.of_string y)
         | _ -> raise (Failure "Unexpected input format"))

(* Perform the task *)
let mainA input =
  input |> List.unzip
  |> (fun (xs, ys) ->
  List.zip_exn
    (List.sort xs ~compare:Int.compare)
    (List.sort ys ~compare:Int.compare))
  |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + abs (x - y))

let mainB input =
  input |> List.unzip |> fun (xs, ys) ->
  List.fold xs ~init:0 ~f:(fun acc x ->
      acc + (x * List.count ys ~f:(fun y -> x = y)))

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> parseInput |> mainA |> stringifyOutput
let b input = input |> parseInput |> mainB |> stringifyOutput
