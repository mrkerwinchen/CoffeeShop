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
  cups_of_coffee : int;
}

(* [customer] is the customer with set preference for coffee *)
type customer = { max_price : float }

(* [customer] is the list customer with varying preferences for coffee *)
type customers = customer list

type day = { cash : float; inventory : inventory; coffee_quantity : int }

(* [state] is the state of the game. *)
type state = {
  day : int;
  recipe : coffee;
  inventory : inventory;
  customers : customers;
  ai : int;
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