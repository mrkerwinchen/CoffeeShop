open State

(** testing*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Coffee Shop.\n";
  print_endline
    "Your goal is to make as much money as possible.\n";
  print_string "> "

  

let () = main ()