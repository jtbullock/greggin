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
let groupTest () =
    let result =
        flattenWithStages
            sourceRecipesMap
            {
                Amount = 1
                Name = "Integrated Logic Circuit"
            }

    let grouped = result |> groupStages

    Assert.IsTrue(grouped.Count = 4)