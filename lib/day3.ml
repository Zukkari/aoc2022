open Base
open Stdio

let char_range a b =
  let lower = Char.to_int a in
  let upper = Char.to_int b in

  List.range lower (upper + 1) |> List.filter_map ~f:Char.of_int

let priority_range a b = List.range a b

let priority (char_lower, char_upper) (score_lower, score_upper) =
  let lower_chars = char_range char_lower char_upper in
  let lower_priorities = priority_range score_lower score_upper in
  List.zip_exn lower_chars lower_priorities

let priorities =
  priority ('a', 'z') (1, 27) @ priority ('A', 'Z') (27, 53)
  |> Map.of_alist_exn (module Char)

let to_rucksacks line =
  let chars = String.to_list line in
  List.length chars |> (fun len -> len / 2) |> List.split_n chars

let intersect_rucksacks (left, right) =
  let unique_left = Set.of_list (module Char) left in
  let unique_right = Set.of_list (module Char) right in
  Set.inter unique_left unique_right

let item_priority i =
  match Map.find priorities i with
  | Some p -> p
  | None -> "No priority for item=" ^ Char.to_string i |> failwith

let rucksack_priority r =
  Set.to_list r |> List.map ~f:item_priority |> List.fold ~init:0 ~f:( + )

let read_lines ~file ~transformer =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f:transformer)

let solve_p1 file =
  let sacks = read_lines ~file ~transformer:to_rucksacks in
  List.map ~f:(fun s -> intersect_rucksacks s |> rucksack_priority) sacks
  |> List.fold ~init:0 ~f:( + )

let intersection group =
  match group with
  | [ t1; t2; t3 ] ->
      let t1_and_t2 = Set.inter t1 t2 in
      let total = Set.inter t1_and_t2 t3 in
      let group_priorities =
        Set.filter_map (module Int) ~f:(fun c -> Map.find priorities c) total
      in
      Set.fold ~init:0 ~f:( + ) group_priorities
  | _ -> "Invalid group" |> failwith

let solve_p2 file =
  read_lines ~file ~transformer:(fun x -> x)
  |> List.groupi ~break:(fun i _ _ -> i % 3 = 0)
  |> List.map ~f:(fun group ->
         List.map
           ~f:(fun sack -> Set.of_list (module Char) (String.to_list sack))
           group)
  |> List.map ~f:intersection |> List.fold ~init:0 ~f:( + )
