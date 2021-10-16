open State

let milk_in_recipe () =
  print_endline "Amount of milk:"

let create_recipe () = 
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nStep 1: Create a Recipe for the Day\n";
  milk_in_recipe () 

let setup ()= create_recipe ()
  
let fail_setup () = print_endline "Oh no, there has been an error"
 

let main () = 
  ANSITerminal.(print_string [red] "\nWelcome to ShopTest!\n");
  print_endline "Your goal is to make as much money as possible";
  print_endline "For each day, you can\n- create a new coffee recipe\n- set your price for a cup of coffee\n- (re)stock your inventory";
  print_endline "At the end of the day, you will see your profit or loss based on the consumers preferences"; 
  print_endline "Type a number 0 to 9 and enter to start";
  match read_line () with
  | number -> if let x = int_of_string number in x >= 0 && x <= 9 then setup () else fail_setup ()


let () = main () 

  

let () = main ()

