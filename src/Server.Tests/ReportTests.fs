module Server.Tests

open NUnit.Framework
open Shared
open Report
open ReportTestData

// [<SetUp>]
// let Setup () = ()

[<Test>]
let flattenWithStagesTest () =
    let result =
        flattenWithStages
            sourceRecipesMap
            {
                Amount = 1
                Name = "Integrated Logic Circuit"
            }

    let crossReference = List.map (fun i -> List.contains i flattenPhaseExpectedOutput) result

    Assert.AreEqual(flattenPhaseExpectedOutput.Length, result.Length)
    Assert.IsTrue(List.forall id crossReference)

[<Test>]
let upsertIngredientToMap_Inserts () =
    let map = Map.ofList [((0, "existing ingredient"), 1.0)]
    let ingredient = ((0, "new ingredient"), 1.0)
    let result = upsertIngredientToMap map ingredient

    let expected = Map.ofList [((0, "existing ingredient"), 1.0); ((0, "new ingredient"), 1.0)]

    Assert.AreEqual(expected, result)

[<Test>]
let upsertIngredientToMap_Updates () =
    let map = Map.ofList [((0, "existing ingredient"), 1.0)]
    let ingredient = ((0, "existing ingredient"), 1.0)
    let result = upsertIngredientToMap map ingredient

    let expected = Map.ofList [((0, "existing ingredient"), 2.0)]

    Assert.AreEqual(expected, result)

[<Test>]

//
// [<Test>]
// let groupTest () =
//     let result =
//         flattenWithStages
//             sourceRecipesMap
//             {
//                 Amount = 1
//                 Name = "Integrated Logic Circuit"
//             }
//
//     let grouped = result |> groupStages
//
//     Assert.IsTrue(grouped.Count = 4)