(**open State*)

type temp = Hot | Cold

type coffee = {
    milk : int;
    sugar : int;
    beans : int;
    price: float;
    temp: temp;
  }

let rec temp_in_recipe () =
  print_endline "Temperature of coffee: 'hot' or 'cold'";
  let string_to_temp = [("hot", Hot); ("cold", Cold)] in
  match read_line () with
  | cmd when cmd = "quit" -> failwith "Thanks for playing" 
  | cmd when cmd = "hot" || cmd =  "cold" -> List.assoc cmd string_to_temp
  | _ -> print_endline "Invalid input, try again"; temp_in_recipe ()

let rec recipe_quantities item = 
  print_endline (item ^ ":");
  match read_line () with
  | cmd when cmd = "quit" -> failwith "Thanks for playing" 
  | number -> try let n = int_of_string number in 
    if n >=0 then n
    else begin print_endline "Invalid number, try again"; recipe_quantities item end 
  with | _ -> print_endline "Invalid input, try again"; recipe_quantities item

let beans_in_recipe () =
  recipe_quantities "Amount of beans"

let sugar_in_recipe () =
  recipe_quantities "Amount of sugar"

let milk_in_recipe () =
  recipe_quantities "Amount of milk" 

let price_of_recipe () =
  float_of_int (recipe_quantities "Price per cup (dollar portion)") +. 
  float_of_int (recipe_quantities "Price per cup (cent portion)") /. 100.

let string_of_temp t = 
  match t with 
  | Hot -> "hot"
  | Cold -> "cold"
let print_recipe {milk; sugar; beans; price; temp;} =
  print_endline (string_of_int milk ^ " milk" );
  print_endline (string_of_int sugar ^ " sugar" );
  print_endline (string_of_int beans ^ " beans" );
  print_endline ("$" ^ string_of_float price ^ " per cup" );
  print_endline (string_of_temp temp ^ " temperature" )

let rec create_recipe x = 
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nStep 1: Create a Recipe for the Day\n";
  let custom_recipe = {
    milk = milk_in_recipe ();
    sugar = sugar_in_recipe ();
    beans = beans_in_recipe ();
    price = price_of_recipe ();
    temp = temp_in_recipe ();
  } in 
  print_endline ("cost: $" ^ string_of_float custom_recipe.price);
  print_endline "your recipe is this:";
  print_recipe custom_recipe; 
  print_endline "Type 'redo' to redo, otherwise any letter to move on";
  match read_line () with
  | cmd when cmd = "redo" -> create_recipe x
  | _ -> print_endline "next part"


let start_game x = create_recipe x

let rec set_difficulty () = 
  print_endline "Type a number 1 to 3 for the difficulty of the Ai with 3 being most difficult";
  match read_line () with
  | number -> try let x = int_of_string number in if x >= 1 && x <= 3 then start_game x 
  else print_endline "There has been an error setting the difficulty, try again"; set_difficulty ()
with | _ -> print_endline "There has been an error setting the difficulty, try again"; set_difficulty ()

let main () = 
  ANSITerminal.(print_string [red] "\nWelcome to ShopTest!\n");
  print_endline "Your goal is to make more money than the Ai";
  print_endline "For each day, you can\n- create a new coffee recipe\n- set your price for a cup of coffee\n- (re)stock your inventory";
  print_endline "At the end of the day, you will see your profit or loss based on the consumers preferences";
  print_endline "To quit the game type 'quit'"; 
  set_difficulty ()


let () = main () 