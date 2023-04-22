module ReportTestData

open Shared;

let sourceRecipes = [
    {
        Name = "Integrated Logic Circuit"
        Ingredients =
            [
                { Amount = 2.0; Name = "Resistor" }
                { Amount = 1.0; Name = "Steel Plate" }
                { Amount = 2.0; Name = "Vacuum Tube" }
                {
                    Amount = 1.0
                    Name = "Coated Circuit Board"
                }
                {
                    Amount = 3.0
                    Name = "1x Red Alloy Cable"
                }
            ]
    }
    {
        Name = "Resistor"
        Ingredients =
            [
                { Amount = 0.67; Name = "Paper" }
                { Amount = 0.34; Name = "Coal Dust" }
                {
                    Amount = 0.34
                    Name = "1x Copper Wire"
                }
            ]
    }
    {
        Name = "Vacuum Tube"
        Ingredients =
            [
                { Amount = 2.0; Name = "Paper" }
                { Amount = 1.0; Name = "Glass Tube" }
                {
                    Amount = 3.0
                    Name = "1x Copper Wire"
                }
            ]
    }
    {
        Name = "Coated Circuit Board"
        Ingredients =
            [
                { Amount = 0.67; Name = "Sticky Resin" }
                { Amount = 1.0; Name = "Wood Plank" }
            ]
    }
    {
        Name = "1x Red Alloy Cable"
        Ingredients =
            [
                {
                    Amount = 1
                    Name = "1x Red Alloy Wire"
                }
                { Amount = 1; Name = "String" }
                { Amount = 1; Name = "Black Carpet" }
            ]
    }
    {
        Name = "1x Red Alloy Wire"
        Ingredients = [ { Amount = 3; Name = "Redstone" }; { Amount = 1; Name = "Copper Ingot" } ]
    }
]

let sourceRecipesMap =
        List.map (fun r -> (r.Name, r.Ingredients)) sourceRecipes |> Map.ofList

let flattenPhaseExpectedOutput = [
    (1, { Amount = 2.0; Name = "Resistor" })
    (0, { Amount = 1.0; Name = "Steel Plate" })
    (1, { Amount = 2.0; Name = "Vacuum Tube" })
    (1,
     {
         Amount = 1.0
         Name = "Coated Circuit Board"
     })
    (2,
     {
         Amount = 3.0
         Name = "1x Red Alloy Cable"
     })
    (0, { Amount = 0.67 * 2.0; Name = "Paper" })
    (0, { Amount = 0.34 * 2.0; Name = "Coal Dust" })
    (0,
     {
         Amount = 0.34 * 2.0
         Name = "1x Copper Wire"
     })
    (0, { Amount = 2.0 * 2.0; Name = "Paper" })
    (0, { Amount = 1.0 * 2.0; Name = "Glass Tube" })
    (0,
     {
         Amount = 3.0 * 2.0
         Name = "1x Copper Wire"
     })
    (0, { Amount = 0.67; Name = "Sticky Resin" })
    (0, { Amount = 1.0; Name = "Wood Plank" })
    (1,
     {
         Amount = 1.0 * 3.0
         Name = "1x Red Alloy Wire"
     })
    (0, { Amount = 1.0 * 3.0; Name = "String" })
    (0, { Amount = 1.0 * 3.0; Name = "Black Carpet" })
    (0, { Amount = 3.0 * 3.0; Name = "Redstone" })
    (0, { Amount = 1.0 * 3.0; Name = "Copper Ingot" })
    (3,
     {
         Amount = 1.0
         Name = "Integrated Logic Circuit"
     })
]