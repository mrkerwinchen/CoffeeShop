open State
open Matplotlib
open Util
open Ai

let plot_revenue ax user_revenue = Ax.plot ax user_revenue

let plot_end_of_day (state : state) (ai_state : ai_state) path =
  let human_rev, ai_rev = stretch_array state.revenue ai_state.ai_revenue in
  let _, ax = Fig.create_with_ax () in
  Ax.set_title ax ("AI_day: " ^ string_of_int ai_state.ai_day);
  plot_revenue ax (cumsum human_rev);
  plot_revenue ax (cumsum ai_rev);
  Mpl.savefig (path ^ "/day" ^ string_of_int state.day ^ "_rev.png");
  state