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
  cups_of_coffee : int;
}

type customer = { max_price : float }

type customers = customer list

type day = { cash : float; inventory : inventory; coffee_quantity : int }

type state = {
  day : int;
  recipe : coffee;
  inventory : inventory;
  customers : customers;
  ai : int;
}
