module Api

open DataAccess
open FSharp.Data.UnitSystems.SI.UnitNames
open Shared
open System.Text.Json
open Azure.Storage.Blobs
open System.IO
open System.Text
open System.Text

let london = {
    Latitude = 51.5074
    Longitude = 0.1278
}

let getDistanceFromLondon postcode = async {
    if not (Validation.isValidPostcode postcode) then
        failwith "Invalid postcode"

    let! location = getLocation postcode
    let distanceToLondon = getDistanceBetweenPositions location.LatLong london

    return {
        Postcode = postcode
        Location = location
        DistanceToLondon = (distanceToLondon / 1000.<meter>)
    }
}

let getCrimeReport postcode = async {
    if not (Validation.isValidPostcode postcode) then
        failwith "Invalid postcode"

    let! location = getLocation postcode
    let! reports = getCrimesNearPosition location.LatLong

    let crimes =
        reports
        |> Array.countBy (fun r -> r.Category)
        |> Array.sortByDescending snd
        |> Array.map (fun (k, c) -> { Crime = k; Incidents = c })

    return crimes
}

let private asWeatherResponse (weather: Weather.OpenMeteoCurrentWeather.CurrentWeather) = {
    WeatherType = weather.Weathercode |> WeatherType.FromCode
    Temperature = float weather.Temperature
}

let getWeather postcode = async {
    (* Task 4.1 WEATHER: Implement a function that retrieves the weather for
       the given postcode. Use the GeoLocation.getLocation, Weather.getWeatherForPosition and
       asWeatherResponse functions to create and return a WeatherResponse instead of the stub.
       Don't forget to use let! instead of let to "await" the Task. *)
    if not (Validation.isValidPostcode postcode) then
       failwith "Invalid postcode"

    let! location = getLocation postcode
    let! weatherInfo = Weather.getWeatherForPosition location.LatLong

    return asWeatherResponse weatherInfo
}

let toMemoryStream (bytes: byte array) = new MemoryStream( bytes )

let getRecipes = async {
    let storageConnString = "DefaultEndpointsProtocol=https;AccountName=gregginblob;AccountKey=nDHyJevQqHJADdOv9THZsEoYtaXfqS4zEhOXFaEhNsq8AotboemQmD+aDxKnfOStC+FqZj9vepUN+AStGecfwg==;EndpointSuffix=core.windows.net"
    
    let container = BlobContainerClient(storageConnString, "recipes")
    let blockBlob = container.GetBlobClient "recipes.json"

    // Fancier way to do this: https://www.planetgeek.ch/2021/04/22/our-journey-to-f-making-async-understand-tasks/
    let! downloadResult = blockBlob.DownloadAsync () |> Async.AwaitTask

    let recipes = JsonSerializer.Deserialize<Map<string, Ingredient list>> downloadResult.Value.Content
    let recipesList = Map.toArray recipes
    return Array.map (fun r -> { Name = (fst r); Ingredients = (snd r) } ) recipesList 
}

let postRecipe (recipe:Recipe) = async {
    let storageConnString = "DefaultEndpointsProtocol=https;AccountName=gregginblob;AccountKey=nDHyJevQqHJADdOv9THZsEoYtaXfqS4zEhOXFaEhNsq8AotboemQmD+aDxKnfOStC+FqZj9vepUN+AStGecfwg==;EndpointSuffix=core.windows.net"
    
    let container = BlobContainerClient(storageConnString, "recipes")
    let blockBlob = container.GetBlobClient "recipes.json"

    //let recipes = Map.empty<string, Ingredient list>

    //let serialized = JsonSerializer.Serialize(recipes.Add (recipe.Name, recipe.Ingredients))
    //let bytes = System.Text.Encoding.UTF8.GetBytes serialized
    //use stream = new MemoryStream( bytes )

    let empty = Map.empty<string, Ingredient list>
    let withRecipe = Map.add recipe.Name recipe.Ingredients empty
    let serialized = JsonSerializer.Serialize withRecipe
    let bytes = System.Text.Encoding.UTF8.GetBytes serialized
    let memoryStream = toMemoryStream bytes
    blockBlob.Upload (memoryStream, true)


    //blockBlob.Upload new MemoryStream( bytes )

    return Array.empty<Recipe>
}

let dojoApi = {
    GetDistance = getDistanceFromLondon

    (* Task 1.1 CRIME: Bind the getCrimeReport function to the GetCrimes method to
         return crime data. Use the above GetDistance field as an example. *)
    GetCrimes = getCrimeReport

(* Task 4.2 WEATHER: Hook up the weather endpoint to the getWeather function. *)
    GetWeather = getWeather

    PostRecipe = postRecipe

    GetRecipes = getRecipes
}