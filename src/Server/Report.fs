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
            ingredients
            |> List.map (flattenWithStages recipes)
            |> List.concat
        let currentStage = childIngredients |> List.map fst |> List.max |> (+) 1
        (currentStage, ingredient) :: childIngredients
    | None -> [ (0, ingredient) ]

// let initOrAppend (ingredient:Ingredient) mapOption =
//     match mapOption with
//     //| Some list -> ingredient :: list
//     | Some map -> Map.change ingredient.Name (fun mapValue -> Some( returnOrMerge ingredient mapValue )) map
//     | None -> Map.ofList [(ingredient.Name, ingredient.Amount)]
//
// let addToGroupMap (map: Map<int, Map<string, double>>) (stage:int, ingredient:Ingredient) =
//     Map.change stage (fun mapValue -> Some(initOrAppend ingredient mapValue)) map
//
let mapUpsert updateFn data keyValue =
    match keyValue with
    | Some x -> Some(updateFn x data)
    | None -> Some(data)

// let mapUpsert updateFn data keyValue =
//     mapUpsert updateFn id data keyValue

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

let groupByStage (map: Map<(int * string), double>) =
    map
    |> Map.toList
    |> List.fold (fun acc ((stage, name), amt) -> Map.change stage (mapUpsert List.append [{Name=name; Amount=amt}]) acc ) Map.empty<int, Ingredient list>

let runReport recipes ingredient =
    flattenWithStages recipes ingredient
    |> mergeByStageAndName
    |> groupByStage

// let addToGroupMap (map: Map<int, Map<string, double>>) (stage:int, ingredient:Ingredient) =
//     Map.change stage (mapUpsert
//                           (fun (existing: Map<string,double>) (newVal:Ingredient) -> upsertIngredientToMap existing newVal)
//                           (fun (x:Ingredient) -> Map.ofList [(x.Name, x.Amount)])
//                           ingredient)
//                           map

//insert: new Map<string,double>
//upsert: upsertIngredientToMap (ingredient:Ingredient) (map:Map<string, double>)

// let groupStages (data: (int * Ingredient) list): Map<int, Ingredient list> =
//     List.fold addToGroupMap Map.empty<int, Map<string, double>> data




