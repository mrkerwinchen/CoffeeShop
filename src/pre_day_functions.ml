open State
open Printer

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

let rec create_recipe temp =
  let _ = Sys.command "clear" in
  let _ = print_weather temp in
  ANSITerminal.(
    print_string [ cyan ] "\nStep 1: Create a Recipe for the Day. \n");
  print_string
    "This will be how much of each ingredient you will include per cup of \
     coffee.\n";
  let custom_recipe =
    {
      milk = milk_in_recipe ();
      sugar = sugar_in_recipe ();
      beans = beans_in_recipe ();
      price = price_of_recipe ();
      temp = temp_in_recipe ();
    }
  in
  ANSITerminal.(print_string [ cyan ] "your recipe is this:\n");
  print_recipe custom_recipe;
  ANSITerminal.(
    print_string [ magenta ] "Type 'redo' to redo, otherwise ENTER move on");
  match read_line () with
  | cmd when cmd = "redo" -> create_recipe temp
  | _ -> custom_recipe

let rec purchase item money item_price =
  ANSITerminal.(
    print_string [ cyan ]
      ("you have $" ^ string_of_float !money ^ " left to spend.\n"));
  print_endline
    ("Amount of " ^ item ^ " ( $" ^ string_of_float item_price ^ " per unit):");
  match read_line () with
  | cmd when cmd = "quit" -> raise (Quit "Thanks for playing")
  | number -> (
      try
        let n = int_of_string number in
        let cost = float_of_int n *. item_price in
        if n >= 0 && cost < !money then (
          ANSITerminal.(
            print_string [ cyan ]
              ("your total for " ^ item ^ " is $" ^ string_of_float cost ^ "\n"));
          let _ = money := !money -. cost in
          n)
        else (
          ANSITerminal.(
            print_string [ red ]
              "Invalid number or you can't afford that much, try again\n");
          purchase item money item_price)
      with _ ->
        ANSITerminal.(print_string [ red ] "Invalid input, try again\n");
        purchase item money item_price)

let prices = { cups = 0.05; milk = 0.15; sugar = 0.1; beans = 0.25 }

let rec fill_inventory prices (inventory : inventory) temp =
  let _ = Sys.command "clear" in
  let _ = print_weather temp in
  ANSITerminal.(
    print_string [ cyan ] "\nStep 2: Buy Supplies From the Inventory Shop\n");
  let old_inv = inventory in
  let _ = ANSITerminal.(print_string [ cyan ] "your current inventory is:\n") in
  let _ = print_inventory old_inv in
  let money = ref inventory.cash in
  let old_money = !money in
  let new_inv =
    let milk = purchase "milk" money prices.milk in
    let sugar = purchase "sugar" money prices.sugar in
    let cups = purchase "cups" money prices.cups in
    let beans = purchase "beans" money prices.beans in
    {
      milk = old_inv.milk + milk;
      sugar = old_inv.sugar + sugar;
      cups = old_inv.cups + cups;
      beans = old_inv.beans + beans;
      cash = !money;
      total_expense = old_inv.total_expense +. (old_money -. !money);
    }
  in
  ANSITerminal.(print_string [ cyan ] "Your new inventory is:\n");
  print_inventory new_inv;
  ANSITerminal.(
    print_string [ magenta ] "Type 'redo' to redo, otherwise ENTER to move on: ");
  match read_line () with
  | cmd when cmd = "redo" ->
      let old_inv = { old_inv with cash = old_money } in
      fill_inventory prices old_inv temp
  | cmd when cmd = "quit" -> raise (Quit "Thanks for playing")
  | _ -> new_inv
