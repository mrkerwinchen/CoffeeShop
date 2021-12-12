exception Quit of string

type temp = Hot | Cold

type coffee = {
  milk : int;
  sugar : int;
  beans : int;
  price : float;
  temp : temp;
}

type inventory = {
  milk : int;
  sugar : int;
  beans : int;
  cash : float;
  cups : int;
  total_expense : float;
}

type inventory_pricing = {
  milk : float;
  sugar : float;
  beans : float;
  cups : float;
}

type customer = {
  max_price : float;
  min_milk : int;
  min_sugar : int;
  min_beans : int;
  desired_temp : temp;
}

type customers = customer array

type day = { cash : float; inventory : inventory; coffee_quantity : int }

type state = {
  day : int;
  recipe : coffee;
  inventory : inventory;
  customers : customers;
  ai : int;
  revenue : float array;
  temp : float;
}
