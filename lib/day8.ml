open Base
open Stdio

type row = int list [@@deriving show]
type grid = row list
type pos = int * int [@@deriving show]

module Visibility = struct
  let from_left grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge = List.sub ~pos:0 ~len:c row in
    List.for_all ~f:(fun tree -> tree < target) from_edge

  let from_top grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge =
      List.map ~f:(fun row -> List.nth_exn row c) grid |> fun trees ->
      List.take trees r
    in
    List.for_all ~f:(fun tree -> tree < target) from_edge

  let from_right grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge = List.drop row (c + 1) in
    List.for_all ~f:(fun tree -> tree < target) from_edge

  let from_bottom grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge =
      List.map ~f:(fun row -> List.nth_exn row c) grid |> fun trees ->
      List.drop trees (r + 1)
    in
    List.for_all ~f:(fun tree -> tree < target) from_edge
end

module SceneScore = struct
  let up grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge =
      List.map ~f:(fun row -> List.nth_exn row c) grid |> fun trees ->
      List.take trees r |> List.rev
    in
    let scene_score =
      List.take_while ~f:(fun x -> x < target) from_edge |> List.length
    in
    if scene_score = List.length from_edge then scene_score else scene_score + 1

  let down grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge =
      List.map ~f:(fun row -> List.nth_exn row c) grid |> fun trees ->
      List.drop trees (r + 1)
    in
    let scene_score =
      List.take_while ~f:(fun x -> x < target) from_edge |> List.length
    in
    if scene_score = List.length from_edge then scene_score else scene_score + 1

  let left grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge = List.sub ~pos:0 ~len:c row |> List.rev in
    let scene_score =
      List.take_while ~f:(fun x -> x < target) from_edge |> List.length
    in
    if scene_score = List.length from_edge then scene_score else scene_score + 1

  let right grid (r, c) =
    let row = List.nth_exn grid r in
    let target = List.nth_exn row c in
    let from_edge = List.drop row (c + 1) in
    let scene_score =
      List.take_while ~f:(fun x -> x < target) from_edge |> List.length
    in
    if scene_score = List.length from_edge then scene_score else scene_score + 1
end

module Tree = struct
  let is_visible grid pos =
    Visibility.from_left grid pos
    || Visibility.from_top grid pos
    || Visibility.from_right grid pos
    || Visibility.from_bottom grid pos

  let scene_score grid pos =
    SceneScore.left grid pos * SceneScore.right grid pos
    * SceneScore.up grid pos * SceneScore.down grid pos
end

let read_lines ~file ~f =
  In_channel.with_file file ~f:(fun inc ->
      In_channel.input_lines inc |> List.map ~f)

let to_int_list line =
  String.to_list line |> List.map ~f:String.of_char |> List.map ~f:Int.of_string

let solve_p1 file =
  let grid = read_lines ~file ~f:to_int_list in
  List.mapi
    ~f:(fun x row ->
      List.mapi ~f:(fun y _ -> if Tree.is_visible grid (x, y) then 1 else 0) row)
    grid
  |> List.fold ~init:0 ~f:(fun acc row -> acc + List.fold ~init:0 ~f:( + ) row)

let solve_p2 file =
  let grid = read_lines ~file ~f:to_int_list in
  List.mapi
    ~f:(fun x row -> List.mapi ~f:(fun y _ -> Tree.scene_score grid (x, y)) row)
    grid
  |> List.map ~f:(fun row -> List.max_elt ~compare:Int.compare row)
  |> List.filter_map ~f:(fun x -> x)
  |> List.max_elt ~compare:Int.compare
