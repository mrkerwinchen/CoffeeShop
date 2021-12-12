exception Quit of string

(* [temp] is the temperature of the coffee. *)
type temp = Hot | Cold

(* [coffee] is the coffee recipe. *)
type coffee = {
  milk : int;
  sugar : int;
  beans : int;
  price : float;
  temp : temp;
}

(* [inventory] is the inventory of ingredients, coffee, and cash. *)
type inventory = {
  milk : int;
  sugar : int;
  beans : int;
  cash : float;
  cups : int;
  total_expense : float;
}

(* [inventory_pricing] is the price of ingredients, coffee, and cash. *)
type inventory_pricing = {
  milk : float;
  sugar : float;
  beans : float;
  cups : float;
}

(* [customer] is the customer with set preference for coffee *)
type customer = {
  max_price : float;
  min_milk : int;
  min_sugar : int;
  min_beans : int;
  desired_temp : temp;
}

(* [customer] is the list customer with varying preferences for coffee *)
type customers = customer array

type day = { cash : float; inventory : inventory; coffee_quantity : int }

(* [state] is the state of the game. *)
type state = {
  day : int;
  recipe : coffee;
  inventory : inventory;
  customers : customers;
  ai : int;
  revenue : float array;
  temp : float;
}

(*
(* [init_state ai] initializes the game state by setting day to 1, the recipe
   and inventory to the lowest amount when applicable, generates customer list
   for the first day, and sets ai to [ai] difficulty depending on user input *)
val init_state : int -> state

(*[change_recipe st] changes the state [st] when the user changes their recipe*)
val change_recipe : state -> state

(*[change_inventory st] changes the state [st] when the user changes their
  inventory*)
val change_inventory : state -> state


*)