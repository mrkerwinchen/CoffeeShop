open State

let string_of_temp t = match t with Hot -> "hot" | Cold -> "cold"

let print_inventory { milk; sugar; beans; cash; cups } =
  print_endline ("Milk: " ^ string_of_int milk);
  print_endline ("Sugar: " ^ string_of_int sugar);
  print_endline ("Beans: " ^ string_of_int beans);
  print_endline ("Cash: $" ^ string_of_float cash);
  print_endline ("Cups: " ^ string_of_int cups)

let print_recipe { milk; sugar; beans; price; temp } =
  print_endline (string_of_int milk ^ " milk");
  print_endline (string_of_int sugar ^ " sugar");
  print_endline (string_of_int beans ^ " beans");
  print_endline ("$" ^ string_of_float price ^ " per cup");
  print_endline (string_of_temp temp ^ " temperature")
