open State
open Random_gen

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

let hard_init_recipe : coffee = temp_coffee

let easy_create_recipe =
  { milk = 2; sugar = 2; beans = 2; price = 1.; temp = Hot }

let medium_create_recipe = temp_coffee

let hard_create_recipe = temp_coffee

(** testable*)
let max_can_buy_ingr ingr_in_inv ingr_in_recipe =
  if ingr_in_recipe = 0. then float_of_int Int.max_int
  else ingr_in_inv /. ingr_in_recipe

let limit_ingr_ord (inventory : inventory) (recipe : coffee) =
  let max_milk =
    max_can_buy_ingr (float_of_int inventory.milk) (float_of_int recipe.milk)
  in
  let max_sugar =
    max_can_buy_ingr (float_of_int inventory.sugar) (float_of_int recipe.sugar)
  in
  let max_beans =
    max_can_buy_ingr (float_of_int inventory.beans) (float_of_int recipe.beans)
  in
  let max_cups = float_of_int inventory.cups in
  let lst =
    [
      ("milk", max_milk);
      ("sugar", max_sugar);
      ("beans", max_beans);
      ("cups", max_cups);
    ]
  in
  List.sort (fun (_, a) (_, b) -> compare a b) lst

let can_buy_ingr (inventory : inventory) (ingr : string) prices num_units =
  match ingr with
  | "milk" -> inventory.cash -. (float_of_int num_units *. prices.milk) >= 0.
  | "sugar" -> inventory.cash -. (float_of_int num_units *. prices.sugar) >= 0.
  | "beans" -> inventory.cash -. (float_of_int num_units *. prices.beans) >= 0.
  | "cup" -> inventory.cash -. (float_of_int num_units *. prices.cups) >= 0.
  | _ -> raise (Failure "Not ingredient")

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

(** recipe = {10 milk, 1 bean, 1 sugar}
  inventory = {  } *)
let rec hard_fill_inv_helper inventory prices lmt_ingr_lst recipe =
  match lmt_ingr_lst with
  | [] -> inventory
  | (ingr, _) :: t ->
      if can_buy_ingr inventory ingr prices 1 then
        let new_inv = buy_units inventory ingr prices 1 in
        let new_lmt_ingr_lst = limit_ingr_ord inventory recipe in
        hard_fill_inv_helper new_inv prices new_lmt_ingr_lst recipe
      else hard_fill_inv_helper inventory prices t recipe

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

let hard_fill_inventory state prices =
  let lmt_ingr_lst = limit_ingr_ord state.inventory state.recipe in
  hard_fill_inv_helper state.inventory prices lmt_ingr_lst state.recipe

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
  | Hard -> raise (Failure "Implement")

(**[ai_create_recipe] is the recipe ai chooses for the day*)
let ai_create_recipe ai_state =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_create_recipe
  | Medium -> raise (Failure "Implement")
  | Hard -> raise (Failure "Implement")

(**[ai_fill_inventory] is the how ai restocks the inventory for the day*)
let ai_fill_inventory (ai_state : state) prices =
  match int_to_difficulty ai_state.ai with
  | Easy -> easy_fill_inventory ai_state prices
  | Medium -> raise (Failure "Implement")
  | Hard -> hard_fill_inventory ai_state prices

let ai_day state = raise (Failure "Implement")