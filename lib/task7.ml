open Core

(* Parse input string *)
let parseInput input =
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         match String.split line ~on:' ' with
         | [] -> raise (Failure "Incorrect format")
         | x :: xs ->
             ( Int.of_string (String.drop_suffix x 1),
               List.map xs ~f:Int.of_string ))

(* Perform the task *)
let isConstructible target nums =
  let rec construct acc nums =
    match nums with
    | [] -> acc = target
    | x :: xs -> construct (acc + x) xs || construct (acc * x) xs
  in
  match nums with
  | [] -> raise (Failure "Incorrect format")
  | x :: xs -> construct x xs

let mainA inputs =
  inputs
  |> List.filter ~f:(fun (target, nums) -> isConstructible target nums)
  |> List.map ~f:fst |> List.fold ~init:0 ~f:( + )

let isConstructibleWithConcatenation target nums =
  let rec construct acc nums =
    match nums with
    | [] -> acc = target
    | x :: xs ->
        construct (acc + x) xs
        || construct (acc * x) xs
        || construct (Int.of_string (Int.to_string acc ^ Int.to_string x)) xs
  in
  match nums with
  | [] -> raise (Failure "Incorrect format")
  | x :: xs -> construct x xs

let mainB inputs =
  inputs
  |> List.filter ~f:(fun (target, nums) ->
         isConstructibleWithConcatenation target nums)
  |> List.map ~f:fst |> List.fold ~init:0 ~f:( + )

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> parseInput |> mainA |> stringifyOutput
let b input = input |> parseInput |> mainB |> stringifyOutput
