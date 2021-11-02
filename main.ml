open State
open Owl

exception Quit of string

let rec temp_in_recipe () =
  print_endline "Temperature of coffee: 'hot' or 'cold'";
  let string_to_temp = [ ("hot", Hot); ("cold", Cold) ] in
  match read_line () with
  | cmd when cmd = "quit" -> raise (Quit "Thanks for playing")
  | cmd when cmd = "hot" || cmd = "cold" -> List.assoc cmd string_to_temp
  | _ ->
      print_endline "Invalid input, try again";
      temp_in_recipe ()

let rec recipe_quantities item =
  print_endline (item ^ ":");
  match read_line () with
  | cmd when cmd = "quit" -> raise (Quit "Thanks for playing")
  | number -> (
      try
        let n = int_of_string number in
        if n >= 0 then n
        else (
          print_endline "Invalid number, try again";
          recipe_quantities item)
      with _ ->
        print_endline "Invalid input, try again";
        recipe_quantities item)

let beans_in_recipe () = recipe_quantities "Amount of beans"

let sugar_in_recipe () = recipe_quantities "Amount of sugar"

let milk_in_recipe () = recipe_quantities "Amount of milk"

let price_of_recipe () =
  float_of_int (recipe_quantities "Price per cup (dollar portion)")
  +. (float_of_int (recipe_quantities "Price per cup (cent portion)") /. 100.)

let string_of_temp t = match t with Hot -> "hot" | Cold -> "cold"

let print_recipe { milk; sugar; beans; price; temp } =
  print_endline (string_of_int milk ^ " milk");
  print_endline (string_of_int sugar ^ " sugar");
  print_endline (string_of_int beans ^ " beans");
  print_endline ("$" ^ string_of_float price ^ " per cup");
  print_endline (string_of_temp temp ^ " temperature")

let rec create_recipe x =
  let _ = Sys.command "clear" in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nStep 1: Create a Recipe for the Day\n";
  let custom_recipe =
    {
      milk = milk_in_recipe ();
      sugar = sugar_in_recipe ();
      beans = beans_in_recipe ();
      price = price_of_recipe ();
      temp = temp_in_recipe ();
    }
  in
  print_endline "your recipe is this:";
  print_recipe custom_recipe;
  print_endline "Type 'redo' to redo, otherwise any letter to move on";
  match read_line () with
  | cmd when cmd = "redo" -> create_recipe x
  | _ -> custom_recipe

let print_inventory { milk; sugar; beans; cash; cups } =
  print_endline ("Milk: " ^ string_of_int milk);
  print_endline ("Sugar: " ^ string_of_int sugar);
  print_endline ("Beans: " ^ string_of_int beans);
  print_endline ("Cash: $" ^ string_of_float cash);
  print_endline ("Cups: " ^ string_of_int cups)

let rec purchase item money item_price =
  print_endline ("you have $" ^ string_of_float !money ^ " left to spend.");
  print_endline
    ("Amount of " ^ item ^ " ( $" ^ string_of_float item_price ^ " per unit):");
  match read_line () with
  | cmd when cmd = "quit" -> raise (Quit "Thanks for playing")
  | number -> (
      try
        let n = int_of_string number in
        let cost = float_of_int n *. item_price in
        if n >= 0 && cost < !money then (
          print_endline
            ("your total for " ^ item ^ " is $" ^ string_of_float cost);
          let _ = money := !money -. cost in
          n)
        else (
          print_endline
            "Invalid number or you can't afford that much, try again";
          purchase item money item_price)
      with _ ->
        print_endline "Invalid input, try again";
        purchase item money item_price)

let prices = { cups = 0.1; milk = 0.50; sugar = 0.25; beans = 0.75 }

let rec fill_inventory prices (inventory : inventory) =
  let _ = Sys.command "clear" in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nStep 2: Buy Supplies From the Inventory Shop\n";
  let old_inv = inventory in
  let _ = print_endline "your current inventory is:" in
  let _ = print_inventory old_inv in
  let money = ref inventory.cash in
  let old_money = !money in
  let new_inv =
    let milk = purchase "milk" money prices.milk in
    let sugar = purchase "sugar" money prices.sugar in
    let cups = purchase "cups" money prices.cups in
    let beans = purchase "beans" money prices.beans in
    { milk; sugar; cups; beans; cash = !money }
  in
  print_endline "Your new inventory is:";
  print_inventory new_inv;
  print_endline "Type 'redo' to redo, otherwise any letter to move on";
  match read_line () with
  | cmd when cmd = "redo" ->
      let old_inv = { old_inv with cash = old_money } in
      fill_inventory prices old_inv
  | cmd when cmd = "quit" -> raise (Quit "Thanks for playing")
  | _ -> new_inv

let gen_customer_list () =
  Array.init (Stats.uniform_int_rvs ~a:10 ~b:15) (fun _ ->
      {
        max_price =
          Stats.gaussian_rvs
            ~mu:(Stats.uniform_rvs ~a:2. ~b:5.)
            ~sigma:(Stats.uniform_rvs ~a:1. ~b:2.);
        min_milk = Stats.uniform_int_rvs ~a:0 ~b:4;
        min_beans = Stats.uniform_int_rvs ~a:0 ~b:4;
        min_sugar = Stats.uniform_int_rvs ~a:0 ~b:4;
      })

let meet_requirements (customer : customer) (recipe : coffee) : bool =
  customer.max_price > recipe.price
  && customer.min_beans <= recipe.beans
  && customer.min_milk <= recipe.milk
  && customer.min_sugar <= recipe.sugar

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

let initialize_state () =
  {
    day = 0;
    recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    inventory = { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 50. };
    customers = [||];
    ai = -1;
  }

let pre_day state : state =
  let recipe = create_recipe () in
  let inventory = fill_inventory prices state.inventory in
  let customers = gen_customer_list () in
  { state with customers; recipe; inventory }

let start_day state : state =
  let state_ref = ref state in
  let _ = Sys.command "clear" in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nStart of day: Let's sell some coffee!\n";
  let revenue = ref 0. in
  let _ =
    for cust = 0 to Array.length state.customers - 1 do
      let customer = state.customers.(cust) in
      let _ = Unix.sleep 1 in

      if meet_requirements customer state.recipe then
        if enough_supplies !state_ref then
          let _ = revenue := !revenue +. state.recipe.price in
          let _ = state_ref := purchase_coffee !state_ref in
          print_endline "Customer purchased!"
        else print_endline "Customer wanted to buy but you're out of supplies!"
      else print_endline "Customer left without purchase"
    done
  in
  let end_of_day_cash = state.inventory.cash +. !revenue in
  let _ = print_endline "END OF DAY" in
  let _ =
    print_endline
      ("you made revenue $" ^ string_of_float !revenue
     ^ " today, with total cash now $"
      ^ string_of_float end_of_day_cash)
  in
  let _ = print_endline "press any key to prepare for the next day" in
  match read_line () with
  | _ ->
      {
        !state_ref with
        inventory = { !state_ref.inventory with cash = end_of_day_cash };
      }

let rec start_game state = state |> pre_day |> start_day |> start_game

let rec set_difficulty () =
  print_endline
    "Type a number 1 to 3 for the difficulty of the Ai with 3 being most \
     difficult";
  match read_line () with
  | cmd when cmd = "quit" -> raise (Quit "Thanks for Playing")
  | number -> (
      try
        let x = int_of_string number in
        if x >= 1 && x <= 3 then x
        else (
          print_endline
            "There has been an error setting the difficulty, try again";
          set_difficulty ())
      with _ ->
        print_endline
          "There has been an error setting the difficulty, try again";
        set_difficulty ())

let main () =
  let _ = Sys.command "clear" in
  ANSITerminal.(print_string [ red ] "\nWelcome to CoffeeShop!\n");
  print_endline "Your goal is to make more money than the Ai";
  print_endline
    "For each day, you can\n\
     - create a new coffee recipe\n\
     - set your price for a cup of coffee\n\
     - (re)stock your inventory";
  print_endline
    "At the end of the day, you will see your profit or loss based on the \
     consumers preferences";
  print_endline "To quit the game type 'quit'";
  let _ = set_difficulty () in
  () |> initialize_state |> start_game

let _ = main ()
