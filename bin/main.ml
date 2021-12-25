open Game
open State
open Random_gen
open Coffeeux
open Customers
open Pre_day_functions
open Start_day_functions
open Report_plot
open Bot
open Util

let initialize_state () =
  {
    day = 0;
    recipe = { milk = 0; sugar = 0; beans = 0; price = 0.; temp = Hot };
    inventory =
      {
        milk = 0;
        sugar = 0;
        beans = 0;
        cups = 0;
        cash = 100.;
        total_expense = 0.;
      };
    customers = [||];
    ai = -1;
    revenue = [||];
    temp = 0.;
  }

let pre_day (state : state) : state =
  let temp = runif ~a:(-20.) ~b:120. in
  let recipe = create_recipe temp in
  let inventory = fill_inventory prices state.inventory temp in
  let customers = gen_customer_list temp in
  let day = state.day + 1 in
  { state with customers; recipe; inventory; day; temp }

let start_day (state : state) : state =
  let state_ref = ref state in
  let _ = Sys.command "clear" in
  let _ =
    ANSITerminal.(
      print_string [ magenta ] "\nStart of day: Let's sell some coffee!\n")
  in
  let _ = flush stdout in
  let revenue_arr = ref [||] in
  let revenue = ref 0. in
  let _ = iterate_customer state_ref state revenue_arr revenue in
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
  let _ = print_endline "Your barista made a summary report for you..." in
  let _ =
    ANSITerminal.(
      print_string [ cyan ]
        ("press any key to view end-of-day report and prepare for the next day"
       ^ "\n"))
  in
  match read_line () with
  | _ ->
      {
        !state_ref with
        inventory = { !state_ref.inventory with cash = end_of_day_cash };
        revenue = !revenue_arr;
      }

let end_day (ai_state : ai_state) (state : state) =
  let new_ai = ai_day ai_state prices state.temp in
  let path = "reports" in
  let _ = plot_end_of_day state new_ai path in
  let state =
    {
      state with
      inventory =
        {
          state.inventory with
          total_expense = state.inventory.total_expense -. sum_arr state.revenue;
        };
    }
  in
  let report_file = path ^ "/day" ^ string_of_int state.day ^ "_rev.png" in
  let exit_code = Sys.command ("open " ^ report_file) in
  let _ =
    if exit_code != 0 then Sys.command ("wslview " ^ report_file) else 0
  in
  (state, new_ai)

let rec start_game ai_state state =
  let new_state, new_ai = state |> pre_day |> start_day |> end_day ai_state in
  start_game new_ai new_state

let rec set_difficulty () =
  print_endline
    "Type a number 1 to 3 for the difficulty of the bot with 3 being most \
     difficult\n\
     [1: Silly Sam; 2: Modest Missy; 3: Gifted Grandma]";
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
  let diff_level = set_difficulty () in
  () |> initialize_state |> start_game (ai_init_state diff_level)

let _ = main ()
