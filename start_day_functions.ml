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
  ()

let initiate_transaction_ui (state_ref : state ref) (state : state)
    (revenue_arr : float array ref) (revenue : float ref) : unit =
  let _ = initiate_transaction state_ref state revenue_arr revenue in
  let _ =
    ANSITerminal.(print_string [ green ] ("Customer purchased!" ^ "\n"))
  in
  flush stdout

let transaction_fail (revenue_arr : float array ref) : unit =
  let _ = revenue_arr := Array.append !revenue_arr [| 0. |] in
  ()

let out_of_supplies_ui revenue_arr =
  let _ = transaction_fail revenue_arr in
  let _ =
    ANSITerminal.(
      print_string [ yellow ]
        ("Customer wanted to buy but you're out of supplies!" ^ "\n"))
  in
  flush stdout

let no_customer_purchase_ui (revenue_arr : float array ref) : unit =
  let _ = transaction_fail revenue_arr in
  ANSITerminal.(
    print_string [ Bold; red ] ("Customer left without purchase" ^ "\n"));
  flush stdout

let auto_iter_customer (state_ref : state ref) (state : state)
    (revenue_arr : float array ref) (revenue : float ref) (display_idx : int) :
    unit =
  for cust = display_idx + 1 to Array.length state.customers - 1 do
    let customer = state.customers.(cust) in
    if meet_requirements customer state.recipe then
      if enough_supplies !state_ref then
        initiate_transaction state_ref state revenue_arr revenue
      else transaction_fail revenue_arr
    else transaction_fail revenue_arr
  done

let iterate_customer (state_ref : state ref) (state : state)
    (revenue_arr : float array ref) (revenue : float ref) : unit =
  let display_idx = min 10 (Array.length state.customers - 1) in
  for cust = 0 to display_idx do
    let customer = state.customers.(cust) in
    let _ = Unix.sleep 1 in

    if meet_requirements customer state.recipe then
      if enough_supplies !state_ref then
        initiate_transaction_ui state_ref state revenue_arr revenue
      else out_of_supplies_ui revenue_arr
    else no_customer_purchase_ui revenue_arr
  done;
  let _ = Unix.sleep 1 in
  let _ =
    print_endline
      "You leave for the rest of the day while your barista Hardworking Harry \
       keeps serving customers until the day is over..."
  in
  let _ = Unix.sleep 1 in
  let _ = auto_iter_customer state_ref state revenue_arr revenue display_idx in
  ()
