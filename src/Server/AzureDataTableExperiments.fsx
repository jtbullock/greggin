open Azure.Data.Blobs
open System.IO
open System.Text.Json

let storageConnString = "DefaultEndpointsProtocol=https;AccountName=gregginblob;AccountKey=nDHyJevQq- HJADdOv9THZsEoYtaXfqS4zEhOXFaEhNsq8AotboemQmD+aDxKnfOStC+FqZj9vepUN+AStGecfwg==;EndpointSuffix=core.- windows.net"

let container = BlobContainerClient(storageConnString, "recipes")
container.CreateIfNotExists()
let blockBlob = container.getBlobClient "myBlob.txt"

let bytes = System.Text.Encoding.UTF8.GetBytes "Why hello there xD"
use stream = new MemoryStream( bytes )
do blockBlob.Upload(stream)

let testMap = Map.ofList [ ("1", "one"); ("2", "two"); ]
let serialized = JsonSerializer.Serialize(testMap)

// With a tuple value:
let testMap = Map.ofList [ ("first", ("Ing 1", 1)); ("second", ("Ing 1", 2));]
let serialized = JsonSerializer.Serialize testMap
JsonSerializer.Deserialize<Map<string,(string * int)>> serialized

JsonSerializer.Deserialize<Map<string, string>> serialized