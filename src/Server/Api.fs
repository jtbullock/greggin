module Api

open Shared
open System.Text.Json
open Azure.Storage.Blobs
open System.IO
open Microsoft.Extensions.Configuration

let toMemoryStream (bytes: byte array) = new MemoryStream(bytes)

let private getRecipesDict (blobConnString) = async {
    //let storageConnString = "DefaultEndpointsProtocol=https;AccountName=gregginblob;AccountKey=nDHyJevQqHJADdOv9THZsEoYtaXfqS4zEhOXFaEhNsq8AotboemQmD+aDxKnfOStC+FqZj9vepUN+AStGecfwg==;EndpointSuffix=core.windows.net"

    let container = BlobContainerClient(blobConnString, "recipes")
    let blockBlob = container.GetBlobClient "recipes.json"

    // Fancier way to do this: https://www.planetgeek.ch/2021/04/22/our-journey-to-f-making-async-understand-tasks/
    let! downloadResult = blockBlob.DownloadAsync() |> Async.AwaitTask

    return JsonSerializer.Deserialize<Map<string, Ingredient list>> downloadResult.Value.Content
}

let recipeMapToResponseArray (recipes: Map<string, Ingredient list>) =
    recipes
    |> Map.toArray
    |> Array.map (fun r -> {
        Name = (fst r)
        Ingredients = (snd r)
    })

let getRecipes storageConnString = async {
    let! recipes = getRecipesDict storageConnString
    return recipeMapToResponseArray recipes
}

let uploadRecipes storageConnString recipes =
    let container = BlobContainerClient(storageConnString, "recipes")
    let blockBlob = container.GetBlobClient "recipes.json"

    let serialized = JsonSerializer.Serialize recipes
    let bytes = System.Text.Encoding.UTF8.GetBytes serialized
    let memoryStream = toMemoryStream bytes
    blockBlob.Upload(memoryStream, true)


let postRecipe storageConnString (recipe: Recipe) = async {
    let! recipes = getRecipesDict (storageConnString)
    let withRecipe = Map.add recipe.Name recipe.Ingredients recipes

    // TODO: make async and handle errors.
    uploadRecipes storageConnString withRecipe |> ignore

    return recipeMapToResponseArray withRecipe
}

let deleteRecipe storageConnString (recipeName: string) : Recipe array Async = async {
    let! recipes = getRecipesDict (storageConnString)
    let removed = Map.remove recipeName recipes

    // TODO: make async and handle errors.
    uploadRecipes storageConnString removed |> ignore

    return recipeMapToResponseArray removed
}

let dojoApi (config: IConfiguration) = {
    PostRecipe = postRecipe config.["ConnectionStrings:BlobStorage"]
    GetRecipes = (fun _ -> getRecipes config.["ConnectionStrings:BlobStorage"])
    DeleteRecipe = deleteRecipe config.["ConnectionStrings:BlobStorage"]
}