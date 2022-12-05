open Base
open Stdio

type crate = Crate of char
type command = { amount : int; source : int; target : int }

let id id = id

let read_lines ~file ~f =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f)

(*
      [D]    
  [N] [C]    
  [Z] [M] [P]
  1   2   3 
*)
let small_board =
  [
    (1, [ Crate 'N'; Crate 'Z' ]);
    (2, [ Crate 'D'; Crate 'C'; Crate 'M' ]);
    (3, [ Crate 'P' ]);
  ]
  |> Map.of_alist_exn (module Int)

(*
  [J]             [F] [M]            
  [Z] [F]     [G] [Q] [F]            
  [G] [P]     [H] [Z] [S] [Q]        
  [V] [W] [Z] [P] [D] [G] [P]        
  [T] [D] [S] [Z] [N] [W] [B] [N]    
  [D] [M] [R] [J] [J] [P] [V] [P] [J]
  [B] [R] [C] [T] [C] [V] [C] [B] [P]
  [N] [S] [V] [R] [T] [N] [G] [Z] [W]
  1   2   3   4   5   6   7   8   9 
*)

let full_board =
  [
    ( 1,
      [
        Crate 'J';
        Crate 'Z';
        Crate 'G';
        Crate 'V';
        Crate 'T';
        Crate 'D';
        Crate 'B';
        Crate 'N';
      ] );
    ( 2,
      [
        Crate 'F';
        Crate 'P';
        Crate 'W';
        Crate 'D';
        Crate 'M';
        Crate 'R';
        Crate 'S';
      ] );
    (3, [ Crate 'Z'; Crate 'S'; Crate 'R'; Crate 'C'; Crate 'V' ]);
    ( 4,
      [
        Crate 'G';
        Crate 'H';
        Crate 'P';
        Crate 'Z';
        Crate 'J';
        Crate 'T';
        Crate 'R';
      ] );
    ( 5,
      [
        Crate 'F';
        Crate 'Q';
        Crate 'Z';
        Crate 'D';
        Crate 'N';
        Crate 'J';
        Crate 'C';
        Crate 'T';
      ] );
    ( 6,
      [
        Crate 'M';
        Crate 'F';
        Crate 'S';
        Crate 'G';
        Crate 'W';
        Crate 'P';
        Crate 'V';
        Crate 'N';
      ] );
    (7, [ Crate 'Q'; Crate 'P'; Crate 'B'; Crate 'V'; Crate 'C'; Crate 'G' ]);
    (8, [ Crate 'N'; Crate 'P'; Crate 'B'; Crate 'Z' ]);
    (9, [ Crate 'J'; Crate 'P'; Crate 'W' ]);
  ]
  |> Map.of_alist_exn (module Int)

module Command = struct
  let of_string line =
    let line_regex =
      Str.regexp "move \\([0-9]+\\) from \\([0-9]+\\) to \\([0-9]+\\)"
    in
    let group group = Str.matched_group group line |> Int.of_string in
    if Str.string_match line_regex line 0 then
      { amount = group 1; source = group 2; target = group 3 }
    else "Incomplete line=" ^ line |> failwith
end

let rec iterate state ?(reverse = true) =
  let open Option.Let_syntax in
  function
  | { amount; source; target } :: xs ->
      let%bind transfer_from = Map.find state source in
      let%bind transfer_to = Map.find state target in
      let left, right = List.split_n transfer_from amount in
      let updated_target =
        (if reverse then List.rev left else left) @ transfer_to
      in
      let new_state =
        Map.set ~key:target ~data:updated_target state
        |> Map.set ~key:source ~data:right
      in
      iterate ?reverse:(Some reverse) new_state xs
  | _ -> Some state

let solve_p1 board file =
  let commands = read_lines ~file ~f:Command.of_string in
  let solution = iterate board commands in
  match solution with
  | None -> failwith "Failed to solve"
  | Some solved ->
      Map.keys solved
      |> List.filter_map ~f:(fun key -> Map.find solved key)
      |> List.filter_map ~f:List.hd
      |> List.map ~f:(fun (Crate label) -> label)
      |> String.of_char_list

let solve_p2 board file =
  let commands = read_lines ~file ~f:Command.of_string in
  let solution = iterate board commands ?reverse:(Some false) in
  match solution with
  | None -> failwith "Failed to solve"
  | Some solved ->
      Map.keys solved
      |> List.filter_map ~f:(fun key -> Map.find solved key)
      |> List.filter_map ~f:List.hd
      |> List.map ~f:(fun (Crate label) -> label)
      |> String.of_char_list
