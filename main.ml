open State

let rec milk_in_recipe x =
  print_endline "Amount of milk:";
  match read_line () with
  | x when x = "quit" -> print_endline "Thanks for playing"
  | number -> try let n = int_of_string number in 
    if n >=0 then begin print_endline (string_of_int n) end
    else begin print_endline "Invalid number, try again"; milk_in_recipe x end 
  with | _ -> print_endline "Invalid input, try again"; milk_in_recipe x
  
let create_recipe x = 
  ANSITerminal.print_string [ ANSITerminal.red ]
<<<<<<< HEAD
    "\n\nWelcome to Coffee Shop.\n";
  print_endline
    "Your goal is to make as much money as possible.\n";
  print_string "> "
=======
    "\nStep 1: Create a Recipe for the Day\n";
  milk_in_recipe x
>>>>>>> e2d3add5a84aea77389e01af9eb6f70fe516064b

let start_game x = create_recipe x
  
let main () = 
  ANSITerminal.(print_string [red] "\nWelcome to ShopTest!\n");
  print_endline "Your goal is to make more money than the Ai";
  print_endline "For each day, you can\n- create a new coffee recipe\n- set your price for a cup of coffee\n- (re)stock your inventory";
  print_endline "At the end of the day, you will see your profit or loss based on the consumers preferences";
  print_endline "To quit the game type 'quit'"; 
  print_endline "Type a number 1 to 3 for the difficulty of the Ai with 3 being most difficult";
  match read_line () with
  | number -> let x = int_of_string number in if x >= 1 && x <= 3 then start_game x 
  else print_endline "Oh no, there has been an error setting the difficulty"

let () = main () 

<<<<<<< HEAD
let () = main ()
(**
let main () = 
  ANSITerminal.(print_string [red] "\nWelcome to ShopTest!\n");
  print_endline "Your goal is to make as much money as possible";
  print_endline "For each day, you can\n- create a new coffee recipe\n- set your price for a cup of coffee\n- (re)stock your inventory";
  print_endline "At the end of the day, you will see your profit or loss based on the consumers preferences"; 
  print_endline "Press a enter to start";
  match read_line () with
  | start -> 
  |
let () = main () *)
=======
>>>>>>> e2d3add5a84aea77389e01af9eb6f70fe516064b
