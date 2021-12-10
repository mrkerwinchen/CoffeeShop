open State
open Random_gen
open Customers

exception Quit of string

type diffculty = Easy | Medium | Hard

let int_to_difficulty ai =
  if ai = 0 then Easy else if ai = 1 then Medium else Hard

let temp_coffee = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot }

let temp_inventory = { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 50. }

let easy_runif = runif_disc ~a:1 ~b:5

let easy_init_recipe : coffee =
  let recipe =
    {
      milk = easy_runif;
      sugar = easy_runif;
      beans = easy_runif;
      price = runif ~a:1. ~b:5.;
      temp = Hot;
    }
  in
  if runif_disc ~a:0 ~b:1 = 0 then { recipe with temp = Cold } else recipe

let medium_init_recipe : coffee = temp_coffee

let hard_init_recipe : coffee =
  { milk = 4; sugar = 4; beans = 4; price = 2.; temp = Hot }

let easy_create_recipe = easy_init_recipe

let medium_create_recipe = temp_coffee

let hard_create_recipe = hard_init_recipe

(*testable*)
let buy_units (inv : inventory) (ingr : string) prices num_units =
  match ingr with
  | "milk" ->
      { inv with milk = inv.milk + num_units; cash = inv.cash -. prices.milk }
  | "sugar" ->
      {
        inv with
        sugar = inv.sugar + num_units;
        cash = inv.cash -. prices.sugar;
      }
  | "beans" ->
      {
        inv with
        beans = inv.beans + num_units;
        cash = inv.cash -. prices.beans;
      }
  | "cup" ->
      { inv with cups = inv.cups + num_units; cash = inv.cash -. prices.cups }
  | _ -> raise (Failure "Not ingredient")

(*testable*)
let buy_bulk_units (inv : inventory) prices num_units =
  let total_cost =
    float_of_int num_units
    *. (prices.milk +. prices.sugar +. prices.beans +. prices.cups)
  in
  {
    milk = inv.milk + num_units;
    sugar = inv.sugar + num_units;
    beans = inv.beans + num_units;
    cups = inv.cups + num_units;
    cash = inv.cash -. total_cost;
  }

(*testable*)
let cost_of_one_coffee (ai_state : state) prices =
  let inventory = ai_state.inventory in
  let c_milk = float_of_int inventory.milk *. prices.milk in
  let c_sugar = float_of_int inventory.sugar *. prices.sugar in
  let c_beans = float_of_int inventory.beans *. prices.beans in
  let c_cups = float_of_int inventory.cups *. prices.cups in
  c_milk +. c_sugar +. c_beans +. c_cups

(*testable*)
let num_coffee_can_buy ai_state prices =
  int_of_float (ai_state.inventory.cash /. cost_of_one_coffee ai_state prices)

(*testable*)
let buy_all_ingr ai_state prices =
  let inventory = ai_state.inventory in
  let num_coff = num_coffee_can_buy ai_state prices in
  buy_bulk_units inventory prices num_coff

(*testable*)
let can_buy_ingr (inventory : inventory) (ingr : string) prices num_units =
  match ingr with
  | "milk" -> inventory.cash -. (float_of_int num_units *. prices.milk) >= 0.
  | "sugar" -> inventory.cash -. (float_of_int num_units *. prices.sugar) >= 0.
  | "beans" -> inventory.cash -. (float_of_int num_units *. prices.beans) >= 0.
  | "cup" -> inventory.cash -. (float_of_int num_units *. prices.cups) >= 0.
  | _ -> raise (Failure "Not ingredient")

(*testable*)
let int_to_ingr r_int =
  match r_int with
  | 0 -> "milk"
  | 1 -> "sugar"
  | 2 -> "beans"
  | 3 -> "cups"
  | _ -> raise (Failure "Not Valid Int")

let rec easy_fill_inv_helper inventory prices num_fails =
  if num_fails > 10 then inventory
  else
    let r_ingr = int_to_ingr (runif_disc ~a:0 ~b:3) in
    let r_num_units = runif_disc ~a:1 ~b:20 in
    if can_buy_ingr inventory r_ingr prices r_num_units then
      let new_inv = buy_units inventory r_ingr prices r_num_units in
      easy_fill_inv_helper new_inv prices num_fails
    else easy_fill_inv_helper inventory prices (num_fails + 1)

let easy_fill_inventory state prices =
  easy_fill_inv_helper state.inventory prices 0

let medium_fill_inventory = temp_inventory

let hard_fill_inventory ai_state prices = buy_all_ingr ai_state prices

(**[ai_init_recipe] is ai first recipe choosen by distributions*)
let ai_init_state state =
  {
    day = 0;
    recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    inventory = { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 50. };
    customers = [||];
    ai = state.ai;
    revenue = [||];
  }

let ai_init_recipe (ai_state : state) =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_init_recipe
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_init_recipe

(**[ai_create_recipe] is the recipe ai chooses for the day*)
let ai_create_recipe ai_state =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_create_recipe
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_create_recipe

(**[ai_fill_inventory] is the how ai restocks the inventory for the day*)
let ai_fill_inventory (ai_state : state) prices =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_fill_inventory ai_state prices
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_fill_inventory ai_state prices

let ai_pre_day (ai_state : state) prices =
  let new_recipe = ai_create_recipe ai_state in
  let new_inv =
    ai_fill_inventory { ai_state with recipe = new_recipe } prices
  in
  let customers = gen_customer_list () in
  let new_day = ai_state.day + 1 in
  {
    ai_state with
    day = new_day;
    recipe = new_recipe;
    inventory = new_inv;
    customers;
  }

let ai_start_day ai_state prices = "a"

let rec ai_day ai_state prices = ai_pre_day ai_state prices