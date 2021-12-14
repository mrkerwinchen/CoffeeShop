open State
open Ai
open Util

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

let print_weather temp =
  print_string "╔═════════════════════════════════════════════════════╗\n";
  print_endline
    ("║ Temperature today: "
    ^ (temp |> round_2 |> string_of_float)
    ^ " degrees F | "
    ^ (temp |> fahrenheit_to_celsius |> round_2 |> string_of_float)
    ^ " degrees C ║");
  print_string "╚═════════════════════════════════════════════════════╝"

let string_of_ai_name (ai_state : ai_state) : string =
  let ai_level = ai_state.ai in
  List.assoc ai_level ai_names

let string_of_ai_nickname (ai_state : ai_state) : string =
  let ai_level = ai_state.ai in
  List.assoc ai_level ai_nicknames