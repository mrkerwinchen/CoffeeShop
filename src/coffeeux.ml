let coffeecup () =
  ANSITerminal.(print_string [ Blink ] "\n               (  )   (   )  )");
  print_string "\n                ) (   )  (  (";
  print_string "\n                ( )  (    ) )";
  print_string "\n                _____________";
  print_string "\n               <_____________> ___";
  print_string "\n               |             |/ _ \\";
  print_string "\n               | ";
  ANSITerminal.(print_string [ Bold; cyan ] " Welcome to ");
  print_string "  | | |";
  print_string "\n               | ";
  ANSITerminal.(print_string [ Bold; cyan ] " CoffeeShop!");
  print_string "  |_| |";
  print_string "\n               |             |\\___/";
  print_string "\n               \\_____________/    \n";
  print_string "╔═════════════════════════════════════════════════╗\n";
  print_endline
    "║ Your goal is to make more money than the AI.    ║\n\
     ║ For each day, you can:                          ║\n\
     ║ - create a new coffee recipe                    ║\n\
     ║ - set your price for a cup of coffee            ║\n\
     ║ - (re)stock your inventory                      ║";

  print_endline
    "║ At the end of the day, you will see your profit ║ \n\
     ║ or loss based on the consumers preferences      ║";
  print_endline "╚═════════════════════════════════════════════════╝"
