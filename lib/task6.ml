open Core

type direction = Up | Down | Left | Right

(* Parse input string *)
let parseInput input =
  let lines = input |> String.split_lines in
  let rows = List.length lines
  and cols = List.nth_exn lines 0 |> String.length
  and objects, positionOpt =
    List.foldi lines ~init:([], None) ~f:(fun i acc ->
        String.foldi ~init:acc ~f:(fun j (objects, positionOpt) c ->
            match c with
            | '#' -> ((i, j) :: objects, positionOpt)
            | '^' -> (objects, Some (i, j))
            | _ -> (objects, positionOpt)))
  in
  match positionOpt with
  | None -> raise (Failure "No starting position found.")
  | Some position -> (rows, cols, objects, position)

(* Perform the task *)
let turnRight = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let getNextCell rows cols (r, c) d =
  match d with
  | Up -> if r = 0 then None else Some (r - 1, c)
  | Down -> if r = rows - 1 then None else Some (r + 1, c)
  | Left -> if c = 0 then None else Some (r, c - 1)
  | Right -> if c = cols - 1 then None else Some (r, c + 1)

let compareCells (ar, ac) (br, bc) =
  let dr, dc = (ar - br, ac - bc) in
  if dr < 0 then -1
  else if dr > 0 then 1
  else if dc < 0 then -1
  else if dc > 0 then 1
  else 0

let eqCells x y = compareCells x y = 0
let compareSteps (ar, ac, _) (br, bc, _) = compareCells (ar, ac) (br, bc)

let eqDirection d1 d2 =
  match (d1, d2) with
  | Up, Up | Down, Down | Left, Left | Right, Right -> true
  | _ -> false

let eqSteps (ar, ac, ad) (br, bc, bd) =
  eqCells (ar, ac) (br, bc) && eqDirection ad bd

let getStepsUntilOutOfArea rows cols objects =
  let rec moveStep acc (r, c) d =
    let nextCellOpt = getNextCell rows cols (r, c) d in
    match nextCellOpt with
    | None -> Some (List.rev ((r, c, d) :: acc))
    | Some (nextR, nextC) ->
        if List.exists objects ~f:(eqCells (nextR, nextC)) then
          moveStep acc (r, c) (turnRight d)
        else if List.exists acc ~f:(eqSteps (r, c, d)) then None
        else moveStep ((r, c, d) :: acc) (nextR, nextC) d
  in
  moveStep

let mainA (rows, cols, objects, init) =
  let pathOpt = getStepsUntilOutOfArea rows cols objects [] init Up in
  match pathOpt with
  | None -> raise (Failure "No path found!")
  | Some path ->
      path |> List.dedup_and_sort ~compare:compareSteps |> List.length

let mainB (rows, cols, objects, init) =
  let pathOpt = getStepsUntilOutOfArea rows cols objects [] init Up in
  match pathOpt with
  | None -> raise (Failure "No path found!")
  | Some path ->
      path
      |> List.foldi ~init:[] ~f:(fun i acc (r, c, d) ->
             (* We propose to put a new object in front of (r,c) *)
             let pathSoFar = List.take path i in
             match getNextCell rows cols (r, c) d with
             | None -> acc
             | Some newObject -> (
                 if
                   eqCells newObject init
                   || List.exists pathSoFar ~f:(fun (pr, pc, _) ->
                          eqCells (pr, pc) newObject)
                 then acc
                 else
                   (* Check if placing an object here will cause a self-loop *)
                   match
                     getStepsUntilOutOfArea rows cols (newObject :: objects)
                       (List.rev pathSoFar) (r, c) d
                   with
                   | None -> newObject :: acc
                   | Some _ -> acc))
      |> List.length

(* Stringify the output value for printing *)
let stringifyOutput = Int.to_string

(* Solutions for parts a and b *)
let a input = input |> parseInput |> mainA |> stringifyOutput
let b input = input |> parseInput |> mainB |> stringifyOutput
