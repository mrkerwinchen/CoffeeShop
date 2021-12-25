open State
open Random_gen
open Customers

exception Quit of string

type difficulty = Easy | Medium | Hard

let ai_names = [ (1, "Silly Sam"); (2, "Modest Missy"); (3, "Gifted Grandma") ]

let ai_nicknames = [ (1, "SS"); (2, "MM"); (3, "GG") ]

type ai_state = {
  ai_day : int;
  ai_recipe : coffee;
  ai_inventory : inventory;
  ai_customers : customers;
  ai : int;
  ai_revenue : float array;
  ai_temperature : float;
  ai_previous_inventory : inventory;
}

let int_to_difficulty ai =
  match ai with
  | 1 -> Easy
  | 2 -> Medium
  | 3 -> Hard
  | _ -> raise (Failure "Not a difficulty level")

(*testable*)
let int_to_ingr r_int =
  match r_int with
  | 0 -> "milk"
  | 1 -> "sugar"
  | 2 -> "beans"
  | 3 -> "cups"
  | _ -> raise (Failure "Not Valid Ingr")

(*testable*)
let cost_of_one_coffee (ai_state : ai_state) prices =
  let recipe = ai_state.ai_recipe in
  let c_milk = float_of_int recipe.milk *. prices.milk in
  let c_sugar = float_of_int recipe.sugar *. prices.sugar in
  let c_beans = float_of_int recipe.beans *. prices.beans in
  let c_cups = prices.cups in
  c_milk +. c_sugar +. c_beans +. c_cups

(*testable*)
let max_coffee_can_buy (ai_state : ai_state) prices =
  int_of_float (ai_state.ai_inventory.cash /. cost_of_one_coffee ai_state prices)

let profit_per_cup (ai_state : ai_state) prices =
  let cost_one_coff = cost_of_one_coffee ai_state prices in
  let rev_one_cup = ai_state.ai_recipe.price in
  rev_one_cup -. cost_one_coff

let max_coffee_can_sell (ai_state : ai_state) prices =
  let recp = ai_state.ai_recipe in
  let inv = ai_state.ai_inventory in
  let m_milk = inv.milk / recp.milk in
  let m_sugar = inv.sugar / recp.sugar in
  let m_beans = inv.beans / recp.beans in
  let m_cups = inv.beans / recp.beans in
  let lst = List.sort min [ m_milk; m_sugar; m_beans; m_cups ] in
  List.hd lst

let max_profit_per_day (ai_state : ai_state) prices =
  let num_coff = max_coffee_can_sell ai_state prices in
  let prof_per_cup = profit_per_cup ai_state prices in
  float_of_int num_coff *. prof_per_cup

let easy_init_recipe : coffee =
  let easy_runif = runif_disc ~a:1 ~b:5 in
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

let medium_init_recipe temp : coffee =
  let med_runif = runif_disc ~a:2 ~b:4 in
  let recipe =
    {
      milk = med_runif;
      sugar = med_runif;
      beans = med_runif;
      price = runif ~a:2. ~b:4.;
      temp = Hot;
    }
  in
  let temp_runif = rnorm ~mu:50. ~sigma:10. in
  if temp > temp_runif then { recipe with temp = Cold } else recipe

let hard_init_recipe temp =
  let recipe = { milk = 4; sugar = 4; beans = 4; price = 3.5; temp = Hot } in
  if temp > 50. then { recipe with temp = Cold } else recipe

let lst_rec_by_ingr (recipe : coffee) =
  let milk = recipe.milk in
  let sugar = recipe.sugar in
  let beans = recipe.beans in
  let lst = [ ("milk", milk); ("sugar", sugar); ("beans", beans) ] in
  List.sort (fun (_, a) (_, b) -> compare a b) lst

let incr_ingr_recipe ingr num_units (recipe : coffee) =
  match ingr with
  | "milk" -> { recipe with milk = recipe.milk + num_units }
  | "sugar" -> { recipe with sugar = recipe.sugar + num_units }
  | "beans" -> { recipe with beans = recipe.beans + num_units }
  | _ -> raise (Failure "Not ingredient")

let incr_all_ingr_recipe (recipe : coffee) num_units =
  {
    recipe with
    milk = recipe.milk + num_units;
    sugar = recipe.sugar + num_units;
    beans = recipe.beans + num_units;
  }

let medium_recipe_price (ai_state : ai_state) prices =
  let recipe = ai_state.ai_recipe in
  let cost_one_coff = cost_of_one_coffee ai_state prices in
  let price = cost_one_coff +. (1.25 *. cost_one_coff) in
  { recipe with price }

let rec medium_incr_recipe recipe diff_lst =
  match diff_lst with
  | [] -> recipe
  | [ h ] -> recipe
  | (ingr1, diff1) :: (ingr2, diff2) :: t ->
      if diff2 - diff1 > 0 || diff1 <= 4 then
        let new_rec = incr_ingr_recipe ingr1 1 recipe in
        medium_incr_recipe new_rec ((ingr2, diff2) :: t)
      else medium_incr_recipe recipe ((ingr2, diff2) :: t)

let easy_create_recipe ai_state prices =
  let old_recipe = easy_init_recipe in
  let r_profit_cup = rnorm ~mu:0.5 ~sigma:0.25 in
  let new_price = cost_of_one_coffee ai_state prices +. r_profit_cup in
  { old_recipe with price = new_price }

let medium_create_ingr_recipe ai_state prices =
  let prev_rec = ai_state.ai_recipe in
  let prev_inv = ai_state.ai_previous_inventory in
  let inv = ai_state.ai_inventory in
  let max_coff_can_sell = max_coffee_can_sell ai_state prices in
  let coff_sold = prev_inv.cups - inv.cups in
  let diff_from_max_sold = max_coff_can_sell - coff_sold in
  if diff_from_max_sold = 0 then incr_all_ingr_recipe prev_rec 1
  else
    let lst_rec = lst_rec_by_ingr prev_rec in
    medium_incr_recipe prev_rec lst_rec

let medium_create_recipe ai_state prices =
  let new_rec = medium_create_ingr_recipe ai_state prices in
  let new_st = { ai_state with ai_recipe = new_rec } in
  let new_rec = medium_recipe_price new_st prices in
  if ai_state.ai_temperature < 50. then { new_rec with temp = Hot }
  else { new_rec with temp = Cold }

let hard_create_recipe = hard_init_recipe

(*testable*)
let can_buy_ingr (inventory : inventory) (ingr : string) prices num_units =
  match ingr with
  | "milk" -> inventory.cash -. (float_of_int num_units *. prices.milk) >= 0.
  | "sugar" -> inventory.cash -. (float_of_int num_units *. prices.sugar) >= 0.
  | "beans" -> inventory.cash -. (float_of_int num_units *. prices.beans) >= 0.
  | "cups" -> inventory.cash -. (float_of_int num_units *. prices.cups) >= 0.
  | _ -> raise (Failure "Not ingredient")

(*testable*)
let buy_idv_units (inv : inventory) (ingr : string) prices num_units =
  match ingr with
  | "milk" ->
      let cost = float_of_int num_units *. prices.milk in
      {
        inv with
        milk = inv.milk + num_units;
        cash = inv.cash -. cost;
        total_expense = inv.total_expense +. cost;
      }
  | "sugar" ->
      let cost = float_of_int num_units *. prices.sugar in
      {
        inv with
        sugar = inv.sugar + num_units;
        cash = inv.cash -. cost;
        total_expense = inv.total_expense +. cost;
      }
  | "beans" ->
      let cost = float_of_int num_units *. prices.beans in
      {
        inv with
        beans = inv.beans + num_units;
        cash = inv.cash -. cost;
        total_expense = inv.total_expense +. cost;
      }
  | "cups" ->
      let cost = float_of_int num_units *. prices.cups in
      {
        inv with
        cups = inv.cups + num_units;
        cash = inv.cash -. cost;
        total_expense = inv.total_expense +. cost;
      }
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
    total_expense = total_cost;
  }

(*testable*)
let buy_all_ingr (ai_state : ai_state) prices =
  let inventory = ai_state.ai_inventory in
  let num_coff = max_coffee_can_buy ai_state prices in
  buy_bulk_units inventory prices num_coff

let rec easy_fill_inv_helper inventory prices num_fails =
  if num_fails > 10 then inventory
  else
    let r_ingr = int_to_ingr (runif_disc ~a:0 ~b:3) in
    let r_num_units = runif_disc ~a:1 ~b:20 in
    if can_buy_ingr inventory r_ingr prices r_num_units then
      let new_inv = buy_idv_units inventory r_ingr prices r_num_units in
      easy_fill_inv_helper new_inv prices num_fails
    else easy_fill_inv_helper inventory prices (num_fails + 1)

let easy_fill_inventory (ai_state : ai_state) prices =
  easy_fill_inv_helper ai_state.ai_inventory prices 0

let medium_fill_inventory (ai_state : ai_state) prices =
  buy_all_ingr ai_state prices

let hard_fill_inventory (ai_state : ai_state) prices =
  buy_all_ingr ai_state prices

let ai_init_inventory =
  { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 100.; total_expense = 0. }

(**[ai_init_recipe] is ai first recipe choosen by distributions*)
let ai_init_state diff : ai_state =
  {
    ai_day = 0;
    ai_recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    ai_inventory = ai_init_inventory;
    ai_customers = [||];
    ai = diff;
    ai_revenue = [||];
    ai_temperature = 0.;
    ai_previous_inventory = ai_init_inventory;
  }

let ai_init_recipe (ai_state : ai_state) =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_init_recipe
  | Medium -> medium_init_recipe ai_state.ai_temperature
  | Hard -> hard_init_recipe ai_state.ai_temperature

(**[ai_create_recipe] is the recipe ai chooses for the day*)
let ai_create_recipe (ai_state : ai_state) prices =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_create_recipe ai_state prices
  | Medium -> medium_create_recipe ai_state prices
  | Hard -> hard_create_recipe ai_state.ai_temperature

(**[ai_fill_inventory] is the how ai restocks the inventory for the day*)
let ai_fill_inventory (ai_state : ai_state) prices =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_fill_inventory ai_state prices
  | Medium -> medium_fill_inventory ai_state prices
  | Hard -> hard_fill_inventory ai_state prices

let ai_pre_day (ai_state : ai_state) prices (temp : float) =
  let new_recipe = ai_create_recipe ai_state prices in
  let new_inv =
    ai_fill_inventory { ai_state with ai_recipe = new_recipe } prices
  in
  let customers = gen_customer_list temp in
  let new_day = ai_state.ai_day + 1 in
  {
    ai_state with
    ai_day = new_day;
    ai_recipe = new_recipe;
    ai_inventory = new_inv;
    ai_customers = customers;
    ai_temperature = temp;
    ai_previous_inventory = new_inv;
  }

let ai_enough_supplies (ai_state : ai_state) : bool =
  ai_state.ai_inventory.milk - ai_state.ai_recipe.milk >= 0
  && ai_state.ai_inventory.beans - ai_state.ai_recipe.beans >= 0
  && ai_state.ai_inventory.sugar - ai_state.ai_recipe.sugar >= 0
  && ai_state.ai_inventory.cups - 1 >= 0

let ai_purchase_coffee (ai_state : ai_state) one_coffee_cost money_gained =
  {
    ai_state with
    ai_inventory =
      {
        milk = ai_state.ai_inventory.milk - ai_state.ai_recipe.milk;
        beans = ai_state.ai_inventory.beans - ai_state.ai_recipe.beans;
        sugar = ai_state.ai_inventory.sugar - ai_state.ai_recipe.sugar;
        cups = ai_state.ai_inventory.cups - 1;
        cash = ai_state.ai_inventory.cash -. one_coffee_cost +. money_gained;
        total_expense = ai_state.ai_inventory.total_expense -. money_gained;
      };
  }

let per_customer_rev (ai_state : ai_state) customer prices : ai_state =
  let recipe = ai_state.ai_recipe in
  let revenue_arr = ai_state.ai_revenue in
  if meet_requirements customer recipe && ai_enough_supplies ai_state then
    let one_coff_cost = cost_of_one_coffee ai_state prices in
    let money_gained = recipe.price -. one_coff_cost in
    let new_st = ai_purchase_coffee ai_state one_coff_cost money_gained in
    let new_arr = Array.append revenue_arr [| ai_state.ai_recipe.price |] in
    { new_st with ai_revenue = new_arr }
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

let ai_day (ai_state : ai_state) prices temp =
  let new_st = ai_pre_day ai_state prices temp in
  ai_start_day new_st prices
