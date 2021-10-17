open State

(** testing*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Coffee Shop.\n";
  print_endline
    "Your goal is to make as much money as possible.\n";
  print_string "> "

  

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