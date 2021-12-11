open State
open Random_gen
open Customers

exception Quit of string

type diffculty = Easy | Medium | Hard

type ai_state = {
  ai_day : int;
  ai_recipe : coffee;
  ai_inventory : inventory;
  ai_customers : customers;
  ai : int;
  ai_revenue : float array;
  ai_temperature : float;
}

let int_to_difficulty ai =
  if ai = 1 then Easy else if ai = 2 then Medium else Hard

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
  | "cups" ->
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
let cost_of_one_coffee (ai_state : ai_state) prices =
  let recipe = ai_state.ai_recipe in
  let c_milk = float_of_int recipe.milk *. prices.milk in
  let c_sugar = float_of_int recipe.sugar *. prices.sugar in
  let c_beans = float_of_int recipe.beans *. prices.beans in
  let c_cups = prices.cups in
  c_milk +. c_sugar +. c_beans +. c_cups

(*testable*)
let num_coffee_can_buy (ai_state : ai_state) prices =
  int_of_float (ai_state.ai_inventory.cash /. cost_of_one_coffee ai_state prices)

(*testable*)
let buy_all_ingr (ai_state : ai_state) prices =
  let inventory = ai_state.ai_inventory in
  let num_coff = num_coffee_can_buy ai_state prices in
  buy_bulk_units inventory prices num_coff

(*testable*)
let can_buy_ingr (inventory : inventory) (ingr : string) prices num_units =
  match ingr with
  | "milk" -> inventory.cash -. (float_of_int num_units *. prices.milk) >= 0.
  | "sugar" -> inventory.cash -. (float_of_int num_units *. prices.sugar) >= 0.
  | "beans" -> inventory.cash -. (float_of_int num_units *. prices.beans) >= 0.
  | "cups" -> inventory.cash -. (float_of_int num_units *. prices.cups) >= 0.
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

let easy_fill_inventory (ai_state : ai_state) prices =
  easy_fill_inv_helper ai_state.ai_inventory prices 0

let medium_fill_inventory = temp_inventory

let hard_fill_inventory (ai_state : ai_state) prices =
  buy_all_ingr ai_state prices

(**[ai_init_recipe] is ai first recipe choosen by distributions*)
let ai_init_state diff : ai_state =
  {
    ai_day = 0;
    ai_recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    ai_inventory = { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 50. };
    ai_customers = [||];
    ai = diff;
    ai_revenue = [||];
    ai_temperature = 0.;
  }

let ai_init_recipe (ai_state : ai_state) =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_init_recipe
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_init_recipe

(**[ai_create_recipe] is the recipe ai chooses for the day*)
let ai_create_recipe (ai_state : ai_state) =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_create_recipe
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_create_recipe

(**[ai_fill_inventory] is the how ai restocks the inventory for the day*)
let ai_fill_inventory (ai_state : ai_state) prices =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_fill_inventory ai_state prices
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_fill_inventory ai_state prices

let ai_pre_day (ai_state : ai_state) prices =
  let new_recipe = ai_create_recipe ai_state in
  let new_inv =
    ai_fill_inventory { ai_state with ai_recipe = new_recipe } prices
  in
  let customers = gen_customer_list () in
  let new_day = ai_state.ai_day + 1 in
  {
    ai_state with
    ai_day = new_day;
    ai_recipe = new_recipe;
    ai_inventory = new_inv;
    ai_customers = customers;
  }

let ai_enough_supplies (ai_state : ai_state) : bool =
  ai_state.ai_inventory.milk - ai_state.ai_recipe.milk >= 0
  && ai_state.ai_inventory.beans - ai_state.ai_recipe.beans >= 0
  && ai_state.ai_inventory.sugar - ai_state.ai_recipe.sugar >= 0
  && ai_state.ai_inventory.cups - 1 >= 0

let ai_purchase_coffee (ai_state : ai_state) =
  {
    ai_state with
    ai_inventory =
      {
        ai_state.ai_inventory with
        milk = ai_state.ai_inventory.milk - ai_state.ai_recipe.milk;
        beans = ai_state.ai_inventory.beans - ai_state.ai_recipe.beans;
        sugar = ai_state.ai_inventory.sugar - ai_state.ai_recipe.sugar;
        cups = ai_state.ai_inventory.cups - 1;
      };
  }

let per_customer_rev (ai_state : ai_state) customer prices : ai_state =
  let recipe = ai_state.ai_recipe in
  let inventory = ai_state.ai_inventory in
  let revenue_arr = ai_state.ai_revenue in
  if meet_requirements customer recipe && ai_enough_supplies ai_state then
    let money_gained = recipe.price -. cost_of_one_coffee ai_state prices in
    let new_st = ai_purchase_coffee ai_state in
    let new_inv = { inventory with cash = inventory.cash +. money_gained } in
    let new_arr = Array.append revenue_arr [| ai_state.ai_recipe.price |] in
    { new_st with ai_inventory = new_inv; ai_revenue = new_arr }
  else
    let new_arr = Array.append revenue_arr [| 0. |] in
    { ai_state with ai_revenue = new_arr }

let rec day_rev (ai_state : ai_state) prices customer_lst =
  match customer_lst with
  | [] -> ai_state
  | h :: t -> day_rev (per_customer_rev ai_state h prices) prices t

let ai_start_day (ai_state : ai_state) prices =
  let ai_st = { ai_state with ai_customers = [||]; ai_revenue = [||] } in
  day_rev ai_st prices (Array.to_list ai_state.ai_customers)

let ai_day (ai_state : ai_state) prices =
  let new_st = ai_pre_day ai_state prices in
  ai_start_day new_st prices
