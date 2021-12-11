open State
open Customers

let enough_supplies state : bool =
  state.inventory.milk - state.recipe.milk >= 0
  && state.inventory.beans - state.recipe.beans >= 0
  && state.inventory.sugar - state.recipe.sugar >= 0
  && state.inventory.cups - 1 >= 0

let purchase_coffee (state : state) =
  {
    state with
    inventory =
      {
        state.inventory with
        milk = state.inventory.milk - state.recipe.milk;
        beans = state.inventory.beans - state.recipe.beans;
        sugar = state.inventory.sugar - state.recipe.sugar;
        cups = state.inventory.cups - 1;
      };
  }

let initiate_transaction (state_ref : state ref) (state : state)
    (revenue_arr : float array ref) (revenue : float ref) : unit =
  let _ = revenue := !revenue +. state.recipe.price in
  let _ = revenue_arr := Array.append !revenue_arr [| state.recipe.price |] in
  let _ = state_ref := purchase_coffee !state_ref in
  let _ =
    ANSITerminal.(print_string [ green ] ("Customer purchased!" ^ "\n"))
  in
  flush stdout

let transaction_fail (revenue_arr : float array ref) : unit =
  let _ = revenue_arr := Array.append !revenue_arr [| 0. |] in
  let _ =
    ANSITerminal.(
      print_string [ yellow ]
        ("Customer wanted to buy but you're out of supplies!" ^ "\n"))
  in
  flush stdout

let no_transaction (revenue_arr : float array ref) : unit =
  let _ = revenue_arr := Array.append !revenue_arr [| 0. |] in
  ANSITerminal.(
    print_string [ Bold; red ] ("Customer left without purchase" ^ "\n"));
  flush stdout

let iterate_customer (state_ref : state ref) (state : state)
    (revenue_arr : float array ref) (revenue : float ref) : unit =
  for cust = 0 to Array.length state.customers - 1 do
    let customer = state.customers.(cust) in
    let _ = Unix.sleep 1 in

    if meet_requirements customer state.recipe then
      if enough_supplies !state_ref then
        initiate_transaction state_ref state revenue_arr revenue
      else transaction_fail revenue_arr
    else no_transaction revenue_arr
  done
