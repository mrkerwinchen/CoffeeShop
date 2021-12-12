open State
open Matplotlib
open Util
open Ai
open Printer

let make_x_axis (revenue_arr : float array) =
  let len = Array.length revenue_arr |> float_of_int in
  let seq_array = make_seq_array 1. len [] in
  let x_axis = rescale_map_transform seq_array 0. 8. in
  x_axis

let plot_revenue ax xs user_revenue lab = Ax.plot ax ~label:lab ~xs user_revenue

let plot_expense ax xs user_revenue expense lab style =
  let len = Array.length user_revenue in
  let expense_line = make_rep_array 1 len [] expense in
  Ax.plot ax ~color:Black ~linestyle:style ~label:lab ~xs expense_line

let plot_end_of_day (state : state) (ai_state : ai_state) path =
  Mpl.style_use "ggplot";
  let human_rev, ai_rev = stretch_array state.revenue ai_state.ai_revenue in
  let x_axis = make_x_axis human_rev in
  let _, ax = Fig.create_with_ax () in
  Ax.set_title ax
    ("End of Day " ^ string_of_int ai_state.ai_day ^ " Revenue Report");
  Ax.set_xlabel ax "Hours Since Start of Day";
  Ax.set_ylabel ax "Revenue ($)";
  plot_revenue ax x_axis (cumsum human_rev) "You";
  plot_revenue ax x_axis (cumsum ai_rev) (string_of_ai_name ai_state);
  plot_expense ax x_axis human_rev state.inventory.total_expense
    "Your Total Expense" Dotted;
  plot_expense ax x_axis ai_rev ai_state.ai_inventory.total_expense
    (string_of_ai_nickname ai_state ^ "'s Total Expense")
    (Other "--");
  Ax.legend ax;
  Mpl.savefig (path ^ "/day" ^ string_of_int state.day ^ "_rev.png");
  state