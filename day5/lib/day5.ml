open! Base
open! Core

let example =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
;;

type range =
  { source : int
  ; destination : int
  ; length : int
  }
[@@deriving sexp]

type range' =
  { start : int
  ; end_ : int
  }
[@@deriving sexp]

let to_range' start length : range' = { start; end_ = start + length }

type t =
  { seeds : int list
  ; seed_range : range list
  ; seed_to_soil : range list
  ; soil_to_fertilizer : range list
  ; fertilizer_to_water : range list
  ; water_to_light : range list
  ; light_to_temperature : range list
  ; temperature_to_humidity : range list
  ; humidity_to_location : range list
  }
[@@deriving sexp]

module Parse = struct
  let parse_range = function
    | [ destination; source; length ] -> { source; destination; length }
    | _ -> failwith "Invalid range"
  ;;

  let parse_raw_range raw_range =
    raw_range |> String.split ~on:' ' |> List.map ~f:Int.of_string |> parse_range
  ;;

  let parse_ranges = function
    | _name :: ranges -> List.map ranges ~f:parse_raw_range
    | _ -> failwith "Invalid ranges"
  ;;

  let parse_seeds raw_seeds =
    let pattern = String.Search_pattern.create ": " in
    match String.Search_pattern.split_on pattern raw_seeds with
    | [ _; seed_numbers ] -> List.map (String.split ~on:' ' seed_numbers) ~f:Int.of_string
    | _ -> failwith "Invalid seeds"
  ;;

  let rec parse_seed_range' (current : range List.t) = function
    | [] -> current
    | source :: length :: rest ->
      parse_seed_range' ({ source; destination = source; length } :: current) rest
    | _ -> failwith "Bad seeds"
  ;;

  let sort_ranges_by_destination =
    List.sort ~compare:(fun left right -> Int.compare left.destination right.destination)
  ;;

  let sort_ranges_by_source =
    List.sort ~compare:(fun left right -> Int.compare left.source right.source)
  ;;

  let make_continuous (ranges : range list) =
    let sorted_by_source =
      List.sort ~compare:(fun left right -> Int.compare left.source right.source) ranges
    in
    let _, ranges =
      List.fold sorted_by_source ~init:(0, []) ~f:(fun (current_start, current) range ->
        let new_start = range.source + range.length + 1 in
        let current =
          let current = range :: current in
          match current_start >= range.source with
          | true -> current
          | false ->
            { source = current_start
            ; destination = current_start
            ; length = range.source - 1 - current_start
            }
            :: current
        in
        new_start, current)
    in
    ranges
  ;;

  let parse_seed_range seeds = parse_seed_range' [] seeds

  let parse data =
    let pattern = String.Search_pattern.create "\n\n" in
    match String.Search_pattern.split_on pattern data with
    | [ seeds
      ; seed_to_soil
      ; soil_to_fertilizer
      ; fertilizer_to_water
      ; water_to_light
      ; light_to_temperature
      ; temperature_to_humidity
      ; humidity_to_location
      ] ->
      let s = Fn.compose sort_ranges_by_destination make_continuous in
      let seeds = parse_seeds seeds in
      { seeds
      ; seed_range = parse_seed_range seeds |> s
      ; seed_to_soil = parse_ranges (String.split_lines seed_to_soil) |> s
      ; soil_to_fertilizer = parse_ranges (String.split_lines soil_to_fertilizer) |> s
      ; fertilizer_to_water = parse_ranges (String.split_lines fertilizer_to_water) |> s
      ; water_to_light = parse_ranges (String.split_lines water_to_light) |> s
      ; light_to_temperature = parse_ranges (String.split_lines light_to_temperature) |> s
      ; temperature_to_humidity =
          parse_ranges (String.split_lines temperature_to_humidity) |> s
      ; humidity_to_location = parse_ranges (String.split_lines humidity_to_location) |> s
      }
    | _ -> failwith "Invalid input"
  ;;

  let%expect_test _ =
    let t = parse example in
    print_s [%message (t : t)];
    [%expect
      {|
    (t
     ((seeds (79 14 55 13))
      (seed_range
       (((source 0) (destination 0) (length 54))
        ((source 55) (destination 55) (length 13))
        ((source 69) (destination 69) (length 9))
        ((source 79) (destination 79) (length 14))))
      (seed_to_soil
       (((source 0) (destination 0) (length 49))
        ((source 98) (destination 50) (length 2))
        ((source 50) (destination 52) (length 48))))
      (soil_to_fertilizer
       (((source 15) (destination 0) (length 37))
        ((source 52) (destination 37) (length 2))
        ((source 0) (destination 39) (length 15))))
      (fertilizer_to_water
       (((source 11) (destination 0) (length 42))
        ((source 0) (destination 42) (length 7))
        ((source 53) (destination 49) (length 8))
        ((source 7) (destination 57) (length 4))))
      (water_to_light
       (((source 0) (destination 0) (length 17))
        ((source 25) (destination 18) (length 70))
        ((source 18) (destination 88) (length 7))))
      (light_to_temperature
       (((source 0) (destination 0) (length 44))
        ((source 77) (destination 45) (length 23))
        ((source 64) (destination 68) (length 13))
        ((source 45) (destination 81) (length 19))))
      (temperature_to_humidity
       (((source 69) (destination 0) (length 1))
        ((source 0) (destination 1) (length 69))))
      (humidity_to_location
       (((source 0) (destination 0) (length 55))
        ((source 93) (destination 56) (length 4))
        ((source 56) (destination 60) (length 37)))))) |}]
  ;;
end

let translate (ranges : range list) number : int =
  List.fold_until
    ranges
    ~init:()
    ~f:(fun _ range ->
      let start = range.source in
      let end_ = range.source + range.length in
      match start <= number, number <= end_ with
      | true, true ->
        let offset = number - range.source in
        Stop (range.destination + offset)
      | _ -> Continue ())
    ~finish:(Fn.const number)
;;

let solve1' t seed =
  [ t.seed_to_soil
  ; t.soil_to_fertilizer
  ; t.fertilizer_to_water
  ; t.water_to_light
  ; t.light_to_temperature
  ; t.temperature_to_humidity
  ; t.humidity_to_location
  ]
  |> List.fold ~init:seed ~f:(fun number ranges -> translate ranges number)
;;

let part1 data =
  let t = Parse.parse data in
  t.seeds
  |> List.map ~f:(solve1' t)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

(* let translate (ranges : range list) (seed_range : seed_range) : int = *)
(*   List.fold_until *)
(*     ranges *)
(*     ~init:() *)
(*     ~f:(fun _ range -> *)
(*       let start = range.source in *)
(*       let end_ = range.source + range.length in *)
(*       match start <= number, number <= end_ with *)
(*       | true, true -> *)
(*         let offset = number - range.source in *)
(*         Stop (range.destination + offset) *)
(*       | _ -> Continue ()) *)
(*     ~finish:(Fn.const number) *)
(* ;; *)

let how_to_reach_arrival (arrival : range) (ranges : range list) : range list =
  let ranges = Parse.sort_ranges_by_destination ranges in
  let target = to_range' arrival.source arrival.length in
  List.fold ~init:[] ranges ~f:(fun current range ->
    let range' = to_range' range.destination range.length in
    let start = max target.start range'.start in
    let end_ = min target.end_ range'.end_ in
    if start < end_
    then (
      let source = range.source in
      let destination = start in
      let length = end_ - start in
      { source; destination; length } :: current)
    else current)
  |> Parse.sort_ranges_by_destination
;;

let how_to_reach_arrival' (arrivals : range list) (ranges : range list) : range list =
  let arrivals = Parse.sort_ranges_by_source arrivals in
  List.map arrivals ~f:(fun arrival -> how_to_reach_arrival arrival ranges) |> List.concat
;;

let%expect_test _ =
  let t = Parse.parse example in
  let arrival = List.hd_exn t.humidity_to_location in
  let how_to_reach = how_to_reach_arrival arrival t.temperature_to_humidity in
  let how_to_reach = how_to_reach_arrival' how_to_reach t.light_to_temperature in
  let how_to_reach = how_to_reach_arrival' how_to_reach t.water_to_light in
  let how_to_reach = how_to_reach_arrival' how_to_reach t.fertilizer_to_water in
  let how_to_reach = how_to_reach_arrival' how_to_reach t.soil_to_fertilizer in
  let how_to_reach = how_to_reach_arrival' how_to_reach t.seed_to_soil in
  let how_to_reach = how_to_reach_arrival' how_to_reach t.seed_range in
  let result =
    how_to_reach
    |> List.map ~f:(fun range -> List.range range.source (range.source + range.length))
    |> List.concat
    |> Int.Set.of_list
    |> Set.to_list
    |> List.map ~f:(solve1' t)
  in
  print_s [%message (arrival : range) (how_to_reach : range list) (result : int list)];
  [%expect
    {|
    ((arrival ((source 0) (destination 0) (length 55)))
     (how_to_reach
      (((source 0) (destination 0) (length 1))
       ((source 0) (destination 0) (length 7))
       ((source 0) (destination 0) (length 17))
       ((source 0) (destination 0) (length 17))
       ((source 0) (destination 0) (length 1))
       ((source 0) (destination 0) (length 9))))
     (result 22)) |}]
;;

let part2 data =
  let t = Parse.parse data in
  Parse.parse_seed_range t.seeds
;;

let%expect_test _ =
  print_s [%message (part1 example : int)];
  print_s [%message (part1 Input.data : int)];
  print_s [%message (part2 example : range list)];
  [%expect
    {|
    ("part1 example" 35)
    ("part1 Input.data" 313045984)
    ("part2 example"
     (((source 55) (destination 55) (length 13))
      ((source 79) (destination 79) (length 14)))) |}]
;;

let solve () =
  print_endline [%string "part | example"];
  print_endline [%string "1    | %{part1 example#Int}"];
  (* print_endline [%string "2    | %{part2 example#Int}"]; *)
  print_endline [%string "---------------"];
  print_endline [%string "part | solution"];
  print_endline [%string "1    | %{part1 Input.data#Int}"]
;;
(* print_endline [%string "2    | %{part2 Input.data#Int}"] *)

(* Part 2 ->

   make all ranges continous
   Depth First Search

   0 -> N
*)
