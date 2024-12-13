open Core

(* Parse input string *)
let parseInput input =
  let lines = input |> String.split_lines in
  let spliti, _ = List.findi_exn lines ~f:(fun _ s -> String.(s = "")) in
  ( List.take lines spliti
    |> List.map ~f:(fun line ->
           match String.split ~on:'|' line with
           | [ x; y ] -> (Int.of_string x, Int.of_string y)
           | _ -> raise (Failure "Incorrect format")),
    List.drop lines (spliti + 1)
    |> List.map ~f:(fun line ->
           line |> String.split ~on:',' |> List.map ~f:Int.of_string) )

(* Perform the task *)
let isUpdateInOrder rules update =
  List.for_all rules ~f:(fun (x, y) ->
      let findx = List.findi update ~f:(fun _ -> Int.( = ) x)
      and findy = List.findi update ~f:(fun _ -> Int.( = ) y) in
      match (findx, findy) with
      | Some (xi, _), Some (yi, _) -> xi < yi
      | _ -> true)

let sumMiddleNumbers =
  List.fold ~init:0 ~f:(fun acc l -> acc + List.nth_exn l (List.length l / 2))

let mainA (rules, updates) =
  List.filter updates ~f:(isUpdateInOrder rules) |> sumMiddleNumbers

let rec insertValueIntoUpdateAccordingToRules rules update v =
  match update with
  | [] -> [ v ]
  | x :: xs ->
      if List.exists rules ~f:(fun (a, b) -> a = v && b = x) then v :: x :: xs
      else x :: insertValueIntoUpdateAccordingToRules rules xs v

let fixUpdateAccordingToRules rules =
  List.fold ~init:[] ~f:(insertValueIntoUpdateAccordingToRules rules)

let mainB (rules, updates) =
  List.filter updates ~f:(fun update -> not (isUpdateInOrder rules update))
  |> List.map ~f:(fun update -> fixUpdateAccordingToRules rules update)
  |> sumMiddleNumbers

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> parseInput |> mainA |> stringifyOutput
let b input = input |> parseInput |> mainB |> stringifyOutput
