open Base
open Stdio

let read_lines ~file ~f =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f)

let contains a b =
  List.for_all ~f:(fun elem -> List.exists ~f:(fun other -> other = elem) a) b

let contains_full a b = contains a b || contains b a

let overlap a b =
  List.exists ~f:(fun elem -> List.exists ~f:(fun other -> elem = other) a) b

let to_range line =
  match String.split ~on:'-' line with
  | [ left; right ] ->
      let lower_bound = Int.of_string left in
      let upper_bound = Int.of_string right in
      List.range lower_bound ?stop:(Some `inclusive) upper_bound
  | _ -> "Wrong pair: " ^ line |> failwith

let to_ranges line =
  match String.split ~on:',' line with
  | [ left; right ] -> (to_range left, to_range right)
  | _ -> "Invalid line=" ^ line |> failwith

let solve_p1 file =
  read_lines ~file ~f:to_ranges
  |> List.filter ~f:(fun (left, right) -> contains_full left right)
  |> List.length

let solve_p2 file =
  read_lines ~file ~f:to_ranges
  |> List.filter ~f:(fun (left, right) -> overlap left right)
  |> List.length
