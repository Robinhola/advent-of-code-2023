open! Base
open! Core

let example = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

type type_ =
  | Five_of_a_kind
  | Four_of_a_kind
  | Full_house
  | Three_of_a_kind
  | Two_pair
  | One_pair
  | High_card
[@@deriving sexp]

let type_to_int = function
  | Five_of_a_kind -> 7
  | Four_of_a_kind -> 6
  | Full_house -> 5
  | Three_of_a_kind -> 4
  | Two_pair -> 3
  | One_pair -> 2
  | High_card -> 1
;;

let card_to_int = function
  | 'A' -> 13
  | 'K' -> 12
  | 'Q' -> 11
  | 'J' -> 10
  | 'T' -> 9
  | '9' -> 8
  | '8' -> 7
  | '7' -> 6
  | '6' -> 5
  | '5' -> 4
  | '4' -> 3
  | '3' -> 2
  | '2' -> 1
  | _ -> failwith "card_to_int"
;;

let compare_cards =
  List.compare (fun left right -> Int.compare (card_to_int left) (card_to_int right))
;;

let compare_type left right = Int.compare (type_to_int left) (type_to_int right)

type cards =
  { value : char list
  ; bid : int
  ; type_ : type_
  }
[@@deriving sexp]

let compare left right =
  match compare_type left.type_ right.type_ with
  | 0 -> compare_cards left.value right.value
  | x -> x
;;

let to_type (value : char list) : type_ =
  let card_to_count =
    List.fold value ~init:Char.Map.empty ~f:(fun acc key ->
      match Map.find acc key with
      | None -> Map.set acc ~key ~data:1
      | Some count -> Map.set acc ~key ~data:(count + 1))
    |> Map.data
    |> List.sort ~compare:Int.compare
  in
  match card_to_count with
  | [ 5 ] -> Five_of_a_kind
  | [ 1; 4 ] -> Four_of_a_kind
  | [ 2; 3 ] -> Full_house
  | [ 1; 1; 3 ] -> Three_of_a_kind
  | [ 1; 2; 2 ] -> Two_pair
  | [ 1; 1; 1; 2 ] -> One_pair
  | [ 1; 1; 1; 1; 1 ] -> High_card
  | card ->
    raise_s
      [%message
        "Invalid card" (card : int list) (card_to_count : int list) (value : char list)]
;;

let parse data =
  String.split_lines data
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(function
    | [ card; bid ] ->
      let value = String.to_list card in
      { value; bid = int_of_string bid; type_ = to_type value }
    | _ -> failwith "Invalid input")
;;

let%expect_test _ =
  let t = parse example |> List.sort ~compare in
  print_s [%message (t : cards list)];
  [%expect
    {|
    (t
     (((value (3 2 T 3 K)) (bid 765) (type_ One_pair))
      ((value (K T J J T)) (bid 220) (type_ Two_pair))
      ((value (K K 6 7 7)) (bid 28) (type_ Two_pair))
      ((value (T 5 5 J 5)) (bid 684) (type_ Three_of_a_kind))
      ((value (Q Q Q J A)) (bid 483) (type_ Three_of_a_kind)))) |}]
;;

let solve1 data =
  parse data
  |> List.sort ~compare
  |> List.mapi ~f:(fun i x -> (i + 1) * x.bid)
  |> List.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  print_s [%message (solve1 example : int)];
  print_s [%message (solve1 Input.data : int)];
  [%expect {|
    ("solve1 example" 6440)
    ("solve1 Input.data" 253954294) |}]
;;

(* let%expect_test _ = *)
(*   print_s [%message *)
(*      (to_type [ 'a'; 'b'; 'c'; 'd'; 'e' ] : type_)]; *)

(*   ]; *)

(* ;; *)
open! Base
open! Core

let example = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

type type_ =
  | Five_of_a_kind
  | Four_of_a_kind
  | Full_house
  | Three_of_a_kind
  | Two_pair
  | One_pair
  | High_card
[@@deriving sexp]

let type_to_int = function
  | Five_of_a_kind -> 7
  | Four_of_a_kind -> 6
  | Full_house -> 5
  | Three_of_a_kind -> 4
  | Two_pair -> 3
  | One_pair -> 2
  | High_card -> 1
;;

let card_to_int = function
  | 'A' -> 13
  | 'K' -> 12
  | 'Q' -> 11
  | 'J' -> 10
  | 'T' -> 9
  | '9' -> 8
  | '8' -> 7
  | '7' -> 6
  | '6' -> 5
  | '5' -> 4
  | '4' -> 3
  | '3' -> 2
  | '2' -> 1
  | _ -> failwith "card_to_int"
;;

let compare_cards =
  List.compare (fun left right -> Int.compare (card_to_int left) (card_to_int right))
;;

let compare_type left right = Int.compare (type_to_int left) (type_to_int right)

type cards =
  { value : char list
  ; bid : int
  ; type_ : type_
  }
[@@deriving sexp]

let compare left right =
  match compare_type left.type_ right.type_ with
  | 0 -> compare_cards left.value right.value
  | x -> x
;;

let to_type (value : char list) : type_ =
  let card_to_count =
    List.fold value ~init:Char.Map.empty ~f:(fun acc key ->
      match Map.find acc key with
      | None -> Map.set acc ~key ~data:1
      | Some count -> Map.set acc ~key ~data:(count + 1))
    |> Map.data
    |> List.sort ~compare:Int.compare
  in
  match card_to_count with
  | [ 5 ] -> Five_of_a_kind
  | [ 1; 4 ] -> Four_of_a_kind
  | [ 2; 3 ] -> Full_house
  | [ 1; 1; 3 ] -> Three_of_a_kind
  | [ 1; 2; 2 ] -> Two_pair
  | [ 1; 1; 1; 2 ] -> One_pair
  | [ 1; 1; 1; 1; 1 ] -> High_card
  | card ->
    raise_s
      [%message
        "Invalid card" (card : int list) (card_to_count : int list) (value : char list)]
;;

let parse data =
  String.split_lines data
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(function
    | [ card; bid ] ->
      let value = String.to_list card in
      { value; bid = int_of_string bid; type_ = to_type value }
    | _ -> failwith "Invalid input")
;;

let%expect_test _ =
  let t = parse example |> List.sort ~compare in
  print_s [%message (t : cards list)];
  [%expect
    {|
    (t
     (((value (3 2 T 3 K)) (bid 765) (type_ One_pair))
      ((value (K T J J T)) (bid 220) (type_ Two_pair))
      ((value (K K 6 7 7)) (bid 28) (type_ Two_pair))
      ((value (T 5 5 J 5)) (bid 684) (type_ Three_of_a_kind))
      ((value (Q Q Q J A)) (bid 483) (type_ Three_of_a_kind)))) |}]
;;

let solve1 data =
  parse data
  |> List.sort ~compare
  |> List.mapi ~f:(fun i x -> (i + 1) * x.bid)
  |> List.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  print_s [%message (solve1 example : int)];
  print_s [%message (solve1 Input.data : int)];
  [%expect {|
    ("solve1 example" 6440)
    ("solve1 Input.data" 253954294) |}]
;;

(* let%expect_test _ = *)
(*   print_s [%message *)
(*      (to_type [ 'a'; 'b'; 'c'; 'd'; 'e' ] : type_)]; *)

(*   ]; *)

(* ;; *)
open! Base
open! Core

let example = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

type type_ =
  | Five_of_a_kind
  | Four_of_a_kind
  | Full_house
  | Three_of_a_kind
  | Two_pair
  | One_pair
  | High_card
[@@deriving sexp]

let type_to_int = function
  | Five_of_a_kind -> 7
  | Four_of_a_kind -> 6
  | Full_house -> 5
  | Three_of_a_kind -> 4
  | Two_pair -> 3
  | One_pair -> 2
  | High_card -> 1
;;

let card_to_int = function
  | 'A' -> 13
  | 'K' -> 12
  | 'Q' -> 11
  | 'J' -> 10
  | 'T' -> 9
  | '9' -> 8
  | '8' -> 7
  | '7' -> 6
  | '6' -> 5
  | '5' -> 4
  | '4' -> 3
  | '3' -> 2
  | '2' -> 1
  | _ -> failwith "card_to_int"
;;

let compare_cards =
  List.compare (fun left right -> Int.compare (card_to_int left) (card_to_int right))
;;

let compare_type left right = Int.compare (type_to_int left) (type_to_int right)

type cards =
  { value : char list
  ; bid : int
  ; type_ : type_
  }
[@@deriving sexp]

let compare left right =
  match compare_type left.type_ right.type_ with
  | 0 -> compare_cards left.value right.value
  | x -> x
;;

let to_type (value : char list) : type_ =
  let card_to_count =
    List.fold value ~init:Char.Map.empty ~f:(fun acc key ->
      match Map.find acc key with
      | None -> Map.set acc ~key ~data:1
      | Some count -> Map.set acc ~key ~data:(count + 1))
    |> Map.data
    |> List.sort ~compare:Int.compare
  in
  match card_to_count with
  | [ 5 ] -> Five_of_a_kind
  | [ 1; 4 ] -> Four_of_a_kind
  | [ 2; 3 ] -> Full_house
  | [ 1; 1; 3 ] -> Three_of_a_kind
  | [ 1; 2; 2 ] -> Two_pair
  | [ 1; 1; 1; 2 ] -> One_pair
  | [ 1; 1; 1; 1; 1 ] -> High_card
  | card ->
    raise_s
      [%message
        "Invalid card" (card : int list) (card_to_count : int list) (value : char list)]
;;

let parse data =
  String.split_lines data
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(function
    | [ card; bid ] ->
      let value = String.to_list card in
      { value; bid = int_of_string bid; type_ = to_type value }
    | _ -> failwith "Invalid input")
;;

let%expect_test _ =
  let t = parse example |> List.sort ~compare in
  print_s [%message (t : cards list)];
  [%expect
    {|
    (t
     (((value (3 2 T 3 K)) (bid 765) (type_ One_pair))
      ((value (K T J J T)) (bid 220) (type_ Two_pair))
      ((value (K K 6 7 7)) (bid 28) (type_ Two_pair))
      ((value (T 5 5 J 5)) (bid 684) (type_ Three_of_a_kind))
      ((value (Q Q Q J A)) (bid 483) (type_ Three_of_a_kind)))) |}]
;;

let solve1 data =
  parse data
  |> List.sort ~compare
  |> List.mapi ~f:(fun i x -> (i + 1) * x.bid)
  |> List.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  print_s [%message (solve1 example : int)];
  print_s [%message (solve1 Input.data : int)];
  [%expect {|
    ("solve1 example" 6440)
    ("solve1 Input.data" 253954294) |}]
;;
