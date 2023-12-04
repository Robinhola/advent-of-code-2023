open! Base
open! Core

let example =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
;;

type t =
  { card : int
  ; winning_numbers : Int.Set.t
  ; card_numbers : Int.Set.t
  }
[@@deriving sexp]

let sanitize numbers = numbers |> List.filter_map ~f:Int.of_string_opt |> Int.Set.of_list

let parse_numbers (number_part : string) =
  match String.split ~on:'|' number_part |> List.map ~f:(String.split ~on:' ') with
  | [ winning; on_the_card ] -> sanitize winning, sanitize on_the_card
  | _ -> failwith "Invalid number"
;;

let%expect_test _ =
  print_s
    [%sexp
      (parse_numbers " 41 48 83 86 17 | 83 86  6 31 17  9 48 53" : Int.Set.t * Int.Set.t)];
  [%expect {| ((17 41 48 83 86) (6 9 17 31 48 53 83 86)) |}]
;;

let parse_card (card_part : string) =
  match String.split ~on:' ' card_part |> List.filter ~f:(Fn.non String.is_empty) with
  | [ "Card"; id ] -> Int.of_string id
  | rest ->
    print_s [%message (rest : string list)];
    failwith ("Invalid card: " ^ card_part)
;;

let%expect_test _ =
  print_s [%sexp (parse_card "Card    10" : int)];
  [%expect {| 10 |}]
;;

let parse_line line =
  match String.split ~on:':' line with
  | [ card_part; number_part ] ->
    let winning_numbers, card_numbers = parse_numbers number_part in
    { card = parse_card card_part; card_numbers; winning_numbers }
  | _ -> failwith "Invalid line"
;;

let parse (data : string) : t list = String.split_lines data |> List.map ~f:parse_line

let%expect_test _ =
  print_s [%message (parse example : t list)];
  [%expect
    {|
    ("parse example"
     (((card 1) (winning_numbers (17 41 48 83 86))
       (card_numbers (6 9 17 31 48 53 83 86)))
      ((card 2) (winning_numbers (13 16 20 32 61))
       (card_numbers (17 19 24 30 32 61 68 82)))
      ((card 3) (winning_numbers (1 21 44 53 59))
       (card_numbers (1 14 16 21 63 69 72 82)))
      ((card 4) (winning_numbers (41 69 73 84 92))
       (card_numbers (5 51 54 58 59 76 83 84)))
      ((card 5) (winning_numbers (26 28 32 83 87))
       (card_numbers (12 22 30 36 70 82 88 93)))
      ((card 6) (winning_numbers (13 18 31 56 72))
       (card_numbers (10 11 23 35 36 67 74 77))))) |}]
;;

let part1 (data : string) =
  parse data
  |> List.map ~f:(fun { card_numbers; winning_numbers; _ } ->
    Set.inter winning_numbers card_numbers |> Set.length)
  |> List.filter ~f:(Int.( < ) 0)
  |> List.sum (module Int) ~f:(fun length -> Int.pow 2 (length - 1))
;;

let%expect_test _ =
  print_s [%message (part1 example : int)];
  print_s [%message (part1 Advent_2023_03_input.data : int)];
  [%expect {|
    ("part1 example" 13)
    ("part1 Advent_2023_03_input.data" 22488) |}]
;;

let solve =
  Command.basic
    ~summary:"Solve 2023-12-03"
    (let%map_open.Command () = return () in
     fun () ->
       let part1 = part1 Advent_2023_03_input.data in
       Ascii_table.simple_list_table
         [ "part"; "solution" ]
         [ [ "1"; Int.to_string part1 ] ])
;;
