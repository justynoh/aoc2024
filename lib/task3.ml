open Core

(* Perform the task *)
let mainA input =
  input
  (* Find all substrings starting with "mul(" *)
  |> String.substr_index_all ~may_overlap:true ~pattern:"mul("
  (* Filter to keep only the substrings that contain ")" *)
  |> List.filter_map ~f:(fun i ->
         let following = String.slice input i (i + 12) in
         let j_opt = String.substr_index following ~pattern:")" in
         match j_opt with
         | None -> None
         | Some j -> Some (String.slice input (i + 4) (i + j)))
  (* Filter to keep those only with a single "," and surrounded by numbers *)
  |> List.filter_map ~f:(fun xy ->
         match String.split xy ~on:',' with
         | [ x; y ] -> (
             match (Int.of_string_opt x, Int.of_string_opt y) with
             | Some x, Some y -> Some (x, y)
             | _ -> None)
         | _ -> None)
  |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + (x * y))

let mainB input =
  (* Find consecutive occurrences of "don't()" to "do()", and slice those sections away *)
  input
  |> String.foldi ~init:(true, "") ~f:(fun i (take, current) c ->
         let isDo =
           String.(
             slice input i (Int.min (i + 4) (String.length input)) = "do()")
         and isDont =
           String.(
             slice input i (Int.min (i + 7) (String.length input)) = "don't()")
         in
         let take = if isDo then true else if isDont then false else take in
         ( take,
           if take then String.append current (String.of_char c) else current ))
  |> snd |> mainA

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> mainA |> stringifyOutput
let b input = input |> mainB |> stringifyOutput
