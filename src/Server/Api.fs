module Api

open Shared
open System.Text.Json
open Azure.Storage.Blobs
open System.IO
open Microsoft.Extensions.Configuration
open Report

type Config =
    {
        StorageConnectionString: string
        ContainerName: string
        RecipeFileName: string
    }

    static member FromIConfiguration(config: IConfiguration) = {
        StorageConnectionString = config.["ConnectionStrings:BlobStorage"]
        ContainerName = config.["Storage:ContainerName"]
        RecipeFileName = config.["Storage:RecipeFileName"]
    }

let private toMemoryStream (bytes: byte array) = new MemoryStream(bytes)

let private recipeMapToResponseArray (recipes: Map<string, Ingredient list>) =
    recipes
    |> Map.toArray
    |> Array.map (fun r -> {
        Name = (fst r)
        Ingredients = (snd r)
    })

let private getBlob (config: Config) =
    let container =
        BlobContainerClient(config.StorageConnectionString, config.ContainerName)

    container.GetBlobClient "recipes.json"

let private getRecipesDict (blob: BlobClient) = async {
    // Fancier way to do this: https://www.planetgeek.ch/2021/04/22/our-journey-to-f-making-async-understand-tasks/
    let! downloadResult = blob.DownloadAsync() |> Async.AwaitTask

    return JsonSerializer.Deserialize<Map<string, Ingredient list>> downloadResult.Value.Content
}

let private uploadRecipes (blob: BlobClient) recipes =
    let serialized = JsonSerializer.Serialize recipes
    let bytes = System.Text.Encoding.UTF8.GetBytes serialized
    let memoryStream = toMemoryStream bytes
    blob.Upload(memoryStream, true)

let getRecipes blobClient () = async {
    let! recipes = getRecipesDict blobClient
    return recipeMapToResponseArray recipes
}

let postRecipe blobClient (recipe: Recipe) = async {
    let! recipes = getRecipesDict blobClient
    let withRecipe = Map.add recipe.Name recipe.Ingredients recipes

    // TODO: make async and handle errors.
    uploadRecipes blobClient withRecipe |> ignore

    return recipeMapToResponseArray withRecipe
}

let deleteRecipe blobClient (recipeName: string) : Recipe array Async = async {
    let! recipes = getRecipesDict (blobClient)
    let removed = Map.remove recipeName recipes

    // TODO: make async and handle errors.
    uploadRecipes blobClient removed |> ignore

    return recipeMapToResponseArray removed
}

let getReport blobClient (selection: Ingredient list, excludes: string list) = async {
    let! recipes = getRecipesDict blobClient
    let tempReportRecipeName = "__report-request__"
    let recipesWithRequest = Map.add tempReportRecipeName selection recipes
    let recipesWithRequestAndExclude = (Map.filter (fun (key:string) _ -> not (List.contains key excludes)) recipesWithRequest)

    let report = runReport recipesWithRequestAndExclude { Name = tempReportRecipeName; Amount = 1}
    return { report with Stages = List.take (report.Stages.Length - 1) report.Stages }
}

let dojoApi (config: IConfiguration) =
    let blob = config |> Config.FromIConfiguration |> getBlob

    {
        PostRecipe = postRecipe blob
        GetRecipes = getRecipes blob
        DeleteRecipe = deleteRecipe blob
        GetReport = getReport blob
    }