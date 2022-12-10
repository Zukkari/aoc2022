open Base
open Stdio

type command = { x : int; remaining : int }
type instruction = NoOp | Addx of command
type registry_state = { cycle : int; x : int; score : int }

let initial_state = { cycle = 1; x = 1; score = 0 }

module Signal = struct
  let strength { cycle; x; _ } =
    match cycle with 20 | 60 | 100 | 140 | 180 | 220 -> cycle * x | _ -> 0
end

module RegistryX = struct
  let iterate instructions =
    let rec aux instructions state =
      let score = Signal.strength state in
      match instructions with
      | x :: xs -> (
          match x with
          | NoOp ->
              let new_state =
                {
                  state with
                  cycle = state.cycle + 1;
                  score = state.score + score;
                }
              in
              aux xs new_state
          | Addx { x; remaining; _ } when remaining = 0 ->
              let new_state =
                {
                  cycle = state.cycle + 1;
                  score = state.score + score;
                  x = state.x + x;
                }
              in
              aux xs new_state
          | Addx { x; _ } ->
              let new_state =
                {
                  state with
                  cycle = state.cycle + 1;
                  score = state.score + score;
                }
              in

              let new_command = Addx { x; remaining = 0 } in

              aux (new_command :: xs) new_state)
      | [] -> score + state.score
    in
    aux instructions initial_state
end

module Instruction = struct
  let of_string = function
    | "noop" -> NoOp
    | other -> (
        match String.split ~on:' ' other with
        | [ _; x ] -> Addx { x = Int.of_string x; remaining = 1 }
        | _ -> "Invalid line" ^ other |> failwith)
end

let read_lines ~file ~f =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f)

let solve_p1 file =
  read_lines ~file ~f:Instruction.of_string |> RegistryX.iterate
