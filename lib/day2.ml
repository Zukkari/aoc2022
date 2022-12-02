open Base
open Stdio

(* A - rock, B - paper, C - scissors *)

type opponent_model = A | B | C

let opponent_of_string = function
  | "A" -> A
  | "B" -> B
  | "C" -> C
  | _ -> failwith "Unknown option"

(* Part 1: X - rock, Y - paper, Z - scissors *)
(* Part 2: X - lose, Y - draw, Z - win *)

type player_model = X | Y | Z

let player_of_string = function
  | "X" -> X
  | "Y" -> Y
  | "Z" -> Z
  | _ -> failwith "Unknown option"

let player_score = function X -> 1 | Y -> 2 | Z -> 3

let calculate_tactits (opponent, player) =
  let calc (opponent, player) =
    match player with
    | X -> ( match opponent with A -> Z | B -> X | C -> Y)
    | Y -> ( match opponent with A -> X | B -> Y | C -> Z)
    | Z -> ( match opponent with A -> Y | B -> Z | C -> X)
  in
  (opponent, calc (opponent, player))

let game_score = function
  | (A, X | B, Y | C, Z) as board ->
      let _, p = board in
      player_score p |> ( + ) 3
  | (A, Y | B, Z | C, X) as board ->
      let _, p = board in
      player_score p |> ( + ) 6
  | _, p -> player_score p

let read_lines ~file ~transformer =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f:transformer)

let of_string line =
  match String.split ~on:' ' line with
  | [ opponent; player ] ->
      (opponent_of_string opponent, player_of_string player)
  | _ -> "Illegal line: " ^ line |> failwith

let solve_p1 file =
  let games = read_lines ~file ~transformer:of_string in
  List.fold ~init:0 ~f:(fun acc board -> acc + game_score board) games

let solve_p2 file =
  let games = read_lines ~file ~transformer:of_string in
  List.fold ~init:0
    ~f:(fun acc board -> acc + (calculate_tactits board |> game_score))
    games
