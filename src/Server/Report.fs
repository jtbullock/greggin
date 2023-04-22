module Report

open Shared

let mapLookup (key: 'a) (map: Map<'a, 'b>) =
    try
        Some(map.[key])
    with _ ->
        None

let getIngredients (recipes: Map<string, Ingredient list>) (recipeName: string) (amount: double) =
    match mapLookup recipeName recipes with
    | Some ingredients ->
        Some(
            List.map
                (fun ingredient ->
                    { ingredient with
                        Amount = ingredient.Amount * amount
                    })
                ingredients
        )
    | None -> None

let rec flattenWithStages (recipes: Map<string, Ingredient list>) (ingredient: Ingredient) =
    match (getIngredients recipes ingredient.Name ingredient.Amount) with
    | Some ingredients ->
        let childIngredients =
            List.map (flattenWithStages recipes) ingredients |> List.concat

        let latestLevel = List.map fst childIngredients |> List.max

        (latestLevel + 1, ingredient) :: childIngredients

    | None -> [ (0, ingredient) ]

let groupMap ingredient x =
    match x with
    | Some x -> Some(ingredient :: x)
    | None -> Some([ingredient])

let addToGroupMap (map: Map<int, Ingredient list>) (stage:int, ingredient:Ingredient) =
    Map.change stage (groupMap ingredient) map

    // if (Map.containsKey stage map) then
    //     Map.change stage (fun v -> ingredient :: v) map
    // else
    //     Map.add stage [ingredient] map

let groupStages (data: (int * Ingredient) list): Map<int, Ingredient list> =
    List.fold addToGroupMap Map.empty<int, Ingredient list> data

