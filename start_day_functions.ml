open State

let enough_supplies state : bool =
  state.inventory.milk - state.recipe.milk >= 0
  && state.inventory.beans - state.recipe.beans >= 0
  && state.inventory.sugar - state.recipe.sugar >= 0
  && state.inventory.cups - 1 >= 0

let purchase_coffee (state : state) =
  {
    state with
    inventory =
      {
        state.inventory with
        milk = state.inventory.milk - state.recipe.milk;
        beans = state.inventory.beans - state.recipe.beans;
        sugar = state.inventory.sugar - state.recipe.sugar;
        cups = state.inventory.cups - 1;
      };
  }
