open Base
open Option
open Stdio

let to_option line =
  match line with "" -> None | other -> Int.of_string other |> some

let group_calories calories =
  let rec group cals curr total =
    match cals with
    | x :: xs -> (
        match x with
        | Some c -> group xs (c :: curr) total
        | None ->
            let total_last = List.fold curr ~init:0 ~f:( + ) in
            group xs [] (total_last :: total))
    | [] ->
        let total_last = List.fold curr ~init:0 ~f:( + ) in
        total_last :: total
  in

  group calories [] []

let read_lines ~file ~transformer =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f:transformer)

let solve_p1 file =
  read_lines ~file ~transformer:to_option
  |> group_calories |> List.fold ~init:0 ~f:max

let solve_p2 file =
  read_lines ~file ~transformer:to_option
  |> group_calories
  |> List.sort ~compare:Int.compare
  |> List.rev
  |> (fun sorted -> List.take sorted 3)
  |> List.fold ~init:0 ~f:( + )
