open OUnit2
open Game
open State

let coffee1 = { milk = 1; sugar = 2; beans = 3; price = 4.3; temp = Hot }

let coffee2 = { milk = 1; sugar = 2; beans = 3; price = 4.3; temp = Cold }

let get_ingr (c : coffee) ingr =
  match ingr with
  | "milk" -> c.milk
  | "sugar" -> c.sugar
  | "beans" -> c.beans
  | _ -> raise (Failure "not int ingr")

let get_price c = match c with { milk; sugar; beans; price; temp } -> price

let get_temp c =
  match c with
  | { milk; sugar; beans; price; temp } -> (
      match temp with Hot -> "hot" | Cold -> "cold")

let coffee_test (name : string) (input : coffee) (ingr : string)
    (expected : int) : test =
  name >:: fun _ -> assert_equal expected (get_ingr input ingr)

let price_test (name : string) (input : coffee) (expected : float) : test =
  name >:: fun _ -> assert_equal expected (get_price input)

let temp_test (name : string) (input : coffee) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (get_temp input)

let coffee_tests =
  [
    coffee_test "get milk" coffee1 "milk" 1;
    coffee_test "get sugar" coffee1 "sugar" 2;
    coffee_test "get beans" coffee1 "beans" 3;
    price_test "get price" coffee1 4.3;
    temp_test "get temp" coffee1 "hot";
    temp_test "get temp" coffee2 "cold";
  ]

let suite = "coffeeshop test suite" >::: coffee_tests

let _ = run_test_tt_main suite