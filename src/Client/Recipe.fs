module Recipe


/// The overall data model driving the view.
type Model = {
    name: string
    ingredients: Set<(string * int)>
}

type Msg =
    | AddIngredient of (string * int)

/// The update function knows how to update the model given a message.
let update msg model =
    match msg with
    | AddIngredient i ->
        { model with ingredients = Set.add i model.ingredients }
