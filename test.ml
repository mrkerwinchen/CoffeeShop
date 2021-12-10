open OUnit2
open Main
open State

let coffee1 = { milk = 3; sugar = 2; beans = 4; price = 4.3; temp = Hot }

let milk (c : coffee) = match c with { milk; _ } -> milk

let coffee_test (name : string) (input : coffee) (expected : int) : test =
  name >:: fun _ -> assert_equal expected (milk input)

let coffee_tests = [ coffee_test "empty state" coffee1 3 ]

let suite = "coffeeshop test suite" >::: coffee_tests

let _ = run_test_tt_main suite