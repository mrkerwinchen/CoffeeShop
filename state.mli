type temp = Hot | Cold

type coffee = {
  milk : int;
  sugar : int;
  beans : int;
  price : float;
  temp : temp;
}

type inventory = { milk : int; sugar : int; beans : int }

type customer = { max_price : float }

type day = { cash : float; inventory : inventory; coffee_quantity : int }