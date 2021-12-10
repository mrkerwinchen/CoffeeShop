open State
open Coffeeux
open Customers
open Pre_day_functions
open Start_day_functions
open Report_plot

let initialize_state () =
  {
    day = 0;
    recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    inventory = { milk = 0; sugar = 0; beans = 0; cups = 0; cash = 50. };
    customers = [||];
    ai = -1;
    revenue = [||];
  }

let pre_day state : state =
  let recipe = create_recipe () in
  let inventory = fill_inventory prices state.inventory in
  let customers = gen_customer_list () in
  let day = state.day + 1 in
  { state with customers; recipe; inventory; day }

let start_day state : state =
  let state_ref = ref state in
  let _ = Sys.command "clear" in
  let _ =
    ANSITerminal.(
      print_string [ magenta ] "\nStart of day: Let's sell some coffee!\n")
  in
  let _ = flush stdout in
  let revenue_arr = ref [||] in
  let revenue = ref 0. in
  let _ =
    for cust = 0 to Array.length state.customers - 1 do
      let customer = state.customers.(cust) in
      let _ = Unix.sleep 1 in

      (if meet_requirements customer state.recipe then
       if enough_supplies !state_ref then
         let _ = revenue := !revenue +. state.recipe.price in
         let _ =
           revenue_arr := Array.append !revenue_arr [| state.recipe.price |]
         in
         let _ = state_ref := purchase_coffee !state_ref in
         let _ =
           ANSITerminal.(print_string [ green ] ("Customer purchased!" ^ "\n"))
         in
         flush stdout
       else
         let _ = revenue_arr := Array.append !revenue_arr [| 0. |] in
         let _ =
           ANSITerminal.(
             print_string [ yellow ]
               ("Customer wanted to buy but you're out of supplies!" ^ "\n"))
         in
         flush stdout
      else
        let _ = revenue_arr := Array.append !revenue_arr [| 0. |] in
        ANSITerminal.(
          print_string [ Bold; red ] ("Customer left without purchase" ^ "\n")));
      flush stdout
    done
  in
  let end_of_day_cash = state.inventory.cash +. !revenue in
  let _ = ANSITerminal.(print_string [ red ] "END OF DAY\n") in
  let _ =
    ANSITerminal.(
      print_string [ magenta ]
        ("you made revenue $" ^ string_of_float !revenue
       ^ " today, with total cash now $"
        ^ string_of_float end_of_day_cash
        ^ "\n"))
  in
  let _ =
    ANSITerminal.(
      print_string [ cyan ] ("press any key to prepare for the next day" ^ "\n"))
  in
  match read_line () with
  | _ ->
      {
        !state_ref with
        inventory = { !state_ref.inventory with cash = end_of_day_cash };
        revenue = !revenue_arr;
      }

let end_day state =
  let path = "reports" in
  let _ = plot_end_of_day state path in
  let report_file = path ^ "/day" ^ string_of_int state.day ^ "_rev.png" in
  let exit_code = Sys.command ("open " ^ report_file) in
  let _ =
    if exit_code != 0 then Sys.command ("wslview " ^ report_file) else 0
  in
  state

let rec start_game state =
  state |> pre_day |> start_day |> end_day |> start_game

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
          ANSITerminal.(
            print_string [ red ]
              "There has been an error setting the difficulty, try again \n");
          set_difficulty ())
      with _ ->
        ANSITerminal.(
          print_string [ red ]
            "There has been an error setting the difficulty, try again\n");
        set_difficulty ())

let main () =
  let _ = Sys.command "rm -fr reports; mkdir -p reports; clear" in
  let _ = coffeecup () in
  ANSITerminal.(
    print_string [ magenta ]
      "To quit the game type 'quit' or ctrl + c at anytime\n");
  let _ = set_difficulty () in
  () |> initialize_state |> start_game

let _ = main ()
