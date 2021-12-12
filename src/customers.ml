open State
open Random_gen

let gen_customer_function temp =
  runif_disc ~a:25 ~b:75
  + int_of_float (20. *. sin (Float.pi *. (temp +. 20.) /. (120. +. 20.)))
  + int_of_float (rnorm ~mu:0. ~sigma:5.)

let gen_customer_list (temp : float) =
  Array.init (gen_customer_function temp) (fun _ ->
      {
        max_price = rnorm ~mu:(runif ~a:2. ~b:5.) ~sigma:(runif ~a:1. ~b:2.);
        min_milk = runif_disc ~a:0 ~b:4;
        min_beans = runif_disc ~a:0 ~b:4;
        min_sugar = runif_disc ~a:0 ~b:4;
      })

let meet_requirements (customer : customer) (recipe : coffee) : bool =
  customer.max_price > recipe.price
  && customer.min_beans <= recipe.beans
  && customer.min_milk <= recipe.milk
  && customer.min_sugar <= recipe.sugar