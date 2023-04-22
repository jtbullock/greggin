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

let rec flattenIntoCraftingStages (recipes: Map<string, Ingredient list>) (ingredient: Ingredient) =
    match (getIngredients recipes ingredient.Name ingredient.Amount) with
    | Some ingredients ->
        let childIngredients =
            ingredients
            |> List.map (flattenIntoCraftingStages recipes)
            |> List.concat
        let currentCraftingStage = childIngredients |> List.map fst |> List.max |> (+) 1
        (currentCraftingStage, ingredient) :: childIngredients
    | None -> [ (0, ingredient) ]

let mapUpsert updateFn data keyValue =
    match keyValue with
    | Some x -> Some(updateFn x data)
    | None -> Some(data)

let groupByStageAndName ((stage: int, ingredient: Ingredient)) =
    ((stage, ingredient.Name), ingredient.Amount)

// If the ingredient isn't in the map, add it.
// If the ingredient is in the map, add our amount to the
// amount in the map.
let upsertIngredientToMap (map:Map<int * string, double>) (key: int * string, amt: double) =
    Map.change key (mapUpsert (+) amt) map

let mergeByStageAndName (reportData: (int * Ingredient) list) =
    reportData
    |> List.map groupByStageAndName
    |> List.fold upsertIngredientToMap Map.empty<int * string, double>
    |> Map.toList
    |> List.map (fun ((stage, name), amt) -> (stage, {Name=name; Amount=amt}))

let groupByStage (ingredientsByStage: (int * Ingredient) list) =
    ingredientsByStage
    |> List.fold
           (fun acc (craftingStage, ingredient) -> Map.change craftingStage (mapUpsert List.append [ingredient]) acc )
           Map.empty<int, Ingredient list>
    |> Map.toList
    |> List.map (fun (stage, ingredients) -> {Stage = stage; Ingredients = ingredients})

let runReport recipes ingredient =
    { Stages =
        flattenIntoCraftingStages recipes ingredient
        |> mergeByStageAndName
        |> groupByStage }
