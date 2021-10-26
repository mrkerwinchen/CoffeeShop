open State

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

let fill_inventory
    ?(prices = { cups = 0.1; milk = 0.50; sugar = 0.25; beans = 0.75 })
    (inventory : inventory) =
  let _ = Sys.command "clear" in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nStep 2: buy supplies from the inventory shop (no refunds)\n";
  let money = ref inventory.cash in
  let new_inv =
    let milk = purchase "milk" money prices.milk in
    let sugar = purchase "sugar" money prices.sugar in
    let cups = purchase "cups" money prices.cups in
    let beans = purchase "beans" money prices.beans in
    { milk; sugar; cups; beans; cash = !money }
  in
  new_inv

let initialize_state () =
  {
    day = 0;
    recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    inventory = { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 50. };
    customers = [];
    ai = -1;
  }

let pre_day state : state =
  let recipe = create_recipe () in
  let inventory = fill_inventory state.inventory in
  { state with customers = []; recipe; inventory }

let rec start_game state = state |> pre_day |> start_game

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
