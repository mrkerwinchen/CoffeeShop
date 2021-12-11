open State

type diffculty = Easy | Medium | Hard

type ai_state = {
  day: int;
  recipe : coffee;
  inventory : inventory;
  customers : customers;
  ai : int;
  revenue : float array;
}