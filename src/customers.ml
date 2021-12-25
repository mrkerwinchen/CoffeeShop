open State
open Random_gen

let gen_customer_function temp =
  runif_disc ~a:25 ~b:75
  + int_of_float (20. *. sin (Float.pi *. (temp +. 20.) /. (120. +. 20.)))
  + int_of_float (rnorm ~mu:0. ~sigma:5.)

let temp_preference_function temp = (temp +. 20.) /. 140.

let temp_preference temp =
  let num = runif_std () in
  let prop = temp_preference_function temp in
  if num > prop then Hot else Cold

let max_price_function temp =
  let pay_extra_thresh = runif ~a:30. ~b:50. in
  let pay_extra_rand = rnorm ~mu:0. ~sigma:1. |> abs_float in
  let std_price = rnorm ~mu:(runif ~a:2. ~b:5.) ~sigma:(runif ~a:1. ~b:2.) in
  if temp < 50. -. pay_extra_thresh || temp > 50. +. pay_extra_thresh then
    std_price +. pay_extra_rand
  else std_price

let gen_customer_list (temp : float) =
  Array.init (gen_customer_function temp) (fun _ ->
      {
        max_price = max_price_function temp;
        min_milk =
          splitdist ~d1:(runif_disc ~a:0 ~b:4) ~p1:0.9
            ~d2:(runif_disc ~a:5 ~b:10) ~p2:0.1;
        min_beans =
          splitdist ~d1:(runif_disc ~a:0 ~b:4) ~p1:0.9
            ~d2:(runif_disc ~a:5 ~b:10) ~p2:0.1;
        min_sugar =
          splitdist ~d1:(runif_disc ~a:0 ~b:4) ~p1:0.9
            ~d2:(runif_disc ~a:5 ~b:10) ~p2:0.1;
        desired_temp = temp_preference temp;
      })

let meet_requirements (customer : customer) (recipe : coffee) : bool =
  customer.max_price > recipe.price
  && customer.min_beans <= recipe.beans
  && customer.min_milk <= recipe.milk
  && customer.min_sugar <= recipe.sugar
  && customer.desired_temp = recipe.temp