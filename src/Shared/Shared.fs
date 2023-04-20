namespace Shared

type Ingredient = { Amount: double; Name: string }

type Recipe = {
    Name: string
    Ingredients: Ingredient list
}

module Route =
    let builder = sprintf "/api/%s/%s"

type IDojoApi = {
    PostRecipe: Recipe -> Recipe array Async
    GetRecipes: unit -> Recipe array Async
    DeleteRecipe: string -> Recipe array Async
}

/// Provides validation on data. Shared across both client and server.
module Validation =
    open System.Text.RegularExpressions

    let isValidPostcode postcode =
        Regex.IsMatch(
            postcode,
            @"([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\s?[0-9][A-Za-z]{2})"
        )