open Core

(* Parse input string *)
let parseInput input =
  input |> String.split_lines |> Array.of_list_map ~f:String.to_array

(* Perform the task *)
let dirsA =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let wordFoundFromPositionInDirection input i j di dj =
  String.to_list "XMAS"
  |> List.for_alli ~f:(fun d c ->
         try
           Char.(Array.get (Array.get input (i + (d * di))) (j + (d * dj)) = c)
         with Invalid_argument _ -> false)

let mainA input =
  Array.foldi input ~init:0 ~f:(fun i count ->
      Array.foldi ~init:count ~f:(fun j count _ ->
          List.fold dirsA ~init:count ~f:(fun count (di, dj) ->
              if wordFoundFromPositionInDirection input i j di dj then count + 1
              else count)))

(* 
  There are only 4 ways for this to happen:
  M.M  M.S  S.M  S.S
  .A.  .A.  .A.  .A.
  S.S  M.S  S.M  M.M
*)

let dirsB =
  [
    [ (-1, -1, 'M'); (-1, 1, 'M'); (0, 0, 'A'); (1, -1, 'S'); (1, 1, 'S') ];
    [ (-1, -1, 'M'); (-1, 1, 'S'); (0, 0, 'A'); (1, -1, 'M'); (1, 1, 'S') ];
    [ (-1, -1, 'S'); (-1, 1, 'M'); (0, 0, 'A'); (1, -1, 'S'); (1, 1, 'M') ];
    [ (-1, -1, 'S'); (-1, 1, 'S'); (0, 0, 'A'); (1, -1, 'M'); (1, 1, 'M') ];
  ]

let mainB input =
  Array.foldi input ~init:0 ~f:(fun i count ->
      Array.foldi ~init:count ~f:(fun j count _ ->
          if
            List.exists dirsB
              ~f:
                (List.for_all ~f:(fun (di, dj, c) ->
                     try
                       Char.(Array.get (Array.get input (i + di)) (j + dj) = c)
                     with Invalid_argument _ -> false))
          then count + 1
          else count))

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> parseInput |> mainA |> stringifyOutput
let b input = input |> parseInput |> mainB |> stringifyOutput
