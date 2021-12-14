open OUnit2
open Game
open State
open Ai

let coffee1 = { milk = 1; sugar = 2; beans = 3; price = 4.3; temp = Hot }

let coffee2 = { milk = 1; sugar = 2; beans = 3; price = 4.3; temp = Cold }

let inv1 =
  {
    milk = 10;
    sugar = 10;
    beans = 10;
    cash = 20.0;
    cups = 10;
    total_expense = 15.0;
  }

let inv2 =
  {
    milk = 7;
    sugar = 7;
    beans = 7;
    cash = 10.0;
    cups = 7;
    total_expense = 10.0;
  }

let cust1 =
  {
    max_price = 4.0;
    min_milk = 1;
    min_sugar = 1;
    min_beans = 2;
    desired_temp = Hot;
  }

let ai1 =
  {
    ai_day = 2;
    ai_recipe = coffee1;
    ai_inventory = inv1;
    ai_customers = [| cust1 |];
    ai = 1;
    ai_revenue = [| 2.0 |];
    ai_temperature = 70.2;
    ai_previous_inventory = inv2;
  }

let inventory_pricing1 = { milk = 0.5; sugar = 0.5; beans = 0.75; cups = 0.25 }

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

let int_to_ingr_test (name : string) (input : int) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (int_to_ingr input)

let cost_one_coffee_test (name : string) (input : ai_state)
    (prices : inventory_pricing) (expected : float) : test =
  name >:: fun _ -> assert_equal expected (cost_of_one_coffee input prices)

let coffee_tests =
  [
    coffee_test "get milk" coffee1 "milk" 1;
    coffee_test "get sugar" coffee1 "sugar" 2;
    coffee_test "get beans" coffee1 "beans" 3;
    price_test "get price" coffee1 4.3;
    temp_test "get temp" coffee1 "hot";
    temp_test "get temp" coffee2 "cold";
    int_to_ingr_test "0 is milk" 0 "milk";
    int_to_ingr_test "1 is sugar" 1 "sugar";
    int_to_ingr_test "2 is beans" 2 "beans";
    int_to_ingr_test "3 is cups" 3 "cups";
    cost_one_coffee_test "cost of " ai1 inventory_pricing1 4.0;
  ]

let suite = "coffeeshop test suite" >::: coffee_tests

let _ = run_test_tt_main suite