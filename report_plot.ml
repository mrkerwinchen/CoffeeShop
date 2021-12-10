open State
open Matplotlib
open Util

let plot_revenue ax user_revenue = Ax.plot ax user_revenue

let plot_end_of_day state path =
  let _, ax = Fig.create_with_ax () in
  plot_revenue ax (cumsum state.revenue);
  Mpl.savefig (path ^ "/day" ^ string_of_int state.day ^ "_rev.png");
  state