open Base
open Stdio

type command = { x : int; remaining : int }
type instruction = NoOp | Addx of command
type registry_state = { cycle : int; x : int; score : int; pixels : char list }

let initial_state = { cycle = 1; x = 1; score = 0; pixels = [] }

module Signal = struct
  let strength { cycle; x; _ } =
    match cycle with 20 | 60 | 100 | 140 | 180 | 220 -> cycle * x | _ -> 0
end

module Pixel = struct
  let is_light pos state =
    List.exists ~f:(( = ) (pos - 1)) [ state.x - 1; state.x; state.x + 1 ]

  let rec normalize pos = if pos <= 40 then pos else normalize (pos - 40)

  let color state =
    let normalized = normalize state.cycle in
    if is_light normalized state then '#' else '.'
end

module RegistryX = struct
  let iterate instructions =
    let rec aux instructions state =
      let score = Signal.strength state in
      let pixel = Pixel.color state in
      match instructions with
      | x :: xs -> (
          match x with
          | NoOp ->
              let new_state =
                {
                  state with
                  cycle = state.cycle + 1;
                  score = state.score + score;
                  pixels = state.pixels @ [ pixel ];
                }
              in
              aux xs new_state
          | Addx { x; remaining; _ } when remaining = 0 ->
              let new_state =
                {
                  cycle = state.cycle + 1;
                  score = state.score + score;
                  x = state.x + x;
                  pixels = state.pixels @ [ pixel ];
                }
              in
              aux xs new_state
          | Addx { x; _ } ->
              let new_state =
                {
                  state with
                  cycle = state.cycle + 1;
                  score = state.score + score;
                  pixels = state.pixels @ [ pixel ];
                }
              in

              let new_command = Addx { x; remaining = 0 } in

              aux (new_command :: xs) new_state)
      | [] -> state
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

module CRT = struct
  let render pixels =
    List.chunks_of pixels ~length:40
    |> List.map ~f:String.of_char_list
    |> String.concat ?sep:(Some "\n")
end

let score { score; _ } = score
let pixels { pixels; _ } = pixels

let read_lines ~file ~f =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f)

let solve file = read_lines ~file ~f:Instruction.of_string |> RegistryX.iterate
let solve_p1 file = solve file |> score
let solve_p2 file = solve file |> pixels |> CRT.render |> print_endline
