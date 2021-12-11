open State
open Matplotlib
open Util
open Ai

let plot_revenue ax user_revenue lab = Ax.plot ax ~label:lab user_revenue

let plot_end_of_day (state : state) (ai_state : ai_state) path =
  Mpl.style_use "ggplot";
  let human_rev, ai_rev = stretch_array state.revenue ai_state.ai_revenue in
  let _, ax = Fig.create_with_ax () in
  Ax.set_title ax
    ("End of Day " ^ string_of_int ai_state.ai_day ^ " Revenue Report");
  Ax.set_xlabel ax "Time of Day";
  Ax.set_ylabel ax "Revenue";
  plot_revenue ax (cumsum human_rev) "You";
  plot_revenue ax (cumsum ai_rev) ("AI Level " ^ string_of_int ai_state.ai);
  Ax.legend ax;
  Mpl.savefig (path ^ "/day" ^ string_of_int state.day ^ "_rev.png");
  state