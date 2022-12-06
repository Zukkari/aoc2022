open Base
open Stdio

module Signal = struct
  let is_unique ?(size = 4) buffer =
    if List.length buffer < size then false
    else
      let unique_chars = Set.of_list (module Char) buffer |> Set.length in
      let total_chars = List.length buffer in
      unique_chars = total_chars

  let append buffer ?(size = 4) i =
    if List.length buffer < size then buffer @ [ i ]
    else
      let cut = List.drop buffer 1 in
      cut @ [ i ]

  let marker_index ?(size = 4) line =
    let rec aux chars buffer pos =
      if is_unique ?size:(Some size) buffer then Some pos
      else
        match chars with
        | x :: xs ->
            let new_buffer = append ?size:(Some size) buffer x in
            aux xs new_buffer (pos + 1)
        | _ -> None
    in
    let chars = String.to_list line in
    aux chars [] 0
end

let read_lines ~file ~f =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f)

let solve_p1 file =
  read_lines ~file ~f:(fun x -> x)
  |> List.filter_map ~f:Signal.marker_index
  |> List.fold ~init:0 ~f:( + )

let solve_p2 file =
  read_lines ~file ~f:(fun x -> x)
  |> List.filter_map ~f:(Signal.marker_index ?size:(Some 14))
  |> List.fold ~init:0 ~f:( + )