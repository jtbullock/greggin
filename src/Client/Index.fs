module Index

open Elmish

open Feliz
open Feliz.Bulma
open Feliz.Recharts
open Feliz.PigeonMaps
open Elmish.SweetAlert
open Fable.Core.JsInterop

open Fable.Remoting.Client
open Shared
open Fable.Core
open Browser.Types

/// The overall data model driving the view.
type Model = {
    Count: int
    Ingredients: (float32 * string) list
}

type Msg =
    | Inc
    | Dec
    | Reset
    | IngredientAdd of (float32 * string)
    | Error of exn
    | RecipeSaved of Recipe array
    | SaveRecipe of Recipe

/// The init function is called to start the message pump with an initial view.
let init () =
    {
        Count = 0
        Ingredients = List.empty
    },
    Cmd.none

let dojoApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IDojoApi>
    
//let saveTheThing recipe = async {
//    dojoApi.PostRecipe recipe
//}

/// The update function knows how to update the model given a message.
let update msg model =
    match msg with
    | Inc -> { model with Count = model.Count + 1 }, Cmd.none
    | Dec -> { model with Count = model.Count - 1 }, Cmd.none
    | IngredientAdd i -> { model with Ingredients = i :: model.Ingredients }, Cmd.none
    | Reset -> init ()
    | Error _ -> model, Cmd.none
    | RecipeSaved _ -> model, Cmd.none
    | SaveRecipe r -> model, Cmd.OfAsync.either dojoApi.PostRecipe r RecipeSaved Error

let card (title:string) (content: ReactElement list) =
    Html.div [
        prop.style [ style.width 600; style.paddingTop 20; style.margin (0, length.auto)]
        prop.children [
            Bulma.box [ prop.children [ Bulma.subtitle title; yield! content ] ]
        ]
    ]

let parseFloat s = try Some (float s) with | _ -> None

let parseFloatDefaultZero s =
    match parseFloat s with
    | Some f -> f
    | None -> 0.0

    //(JS.console.log recipe)

[<ReactComponent>]
let recipeBuilder(onSave) =

    let (ingredients, setIngredients) = React.useState(List.empty<Ingredient>)
    let (recipeName, setRecipeName) = React.useState("")

    let (quantity, setQuantity) = React.useState(0.0)
    let (name, setName) = React.useState("")

    let saveIngredient() =
        setIngredients ({Amount = quantity; Name = name} :: ingredients)
        setQuantity 0.0
        setName ""

    let deleteIngredient (name:string) =
        ingredients
        |> List.filter ( fun ( i ) -> i.Name <> name )
        |> setIngredients

    let handleSubmit (e: Event) = do
        e.preventDefault()
        JS.console.log "Form submitted"

    // There's some READ BAD juju going on here:
    //let saveRecipe =
    //    onSave { Name = recipeName; Ingredients = ingredients }
    // onSave is called every time this re-renders
    // I wonder why? 🤔

    let ingredientToTableRow (ingredient: Ingredient) =
        Html.tr [
            Html.td [ prop.text ingredient.Amount ]
            Html.td [ prop.text ingredient.Name ]
            Html.td [
                Html.button [
                    //prop.text "Delete"
                    prop.type' "button"
                    prop.children [
                        Html.i [ prop.className "fas fa-trash" ]
                    ]
                    prop.onClick (fun _ -> deleteIngredient name )
                ]
            ]
        ]

    let ingredientsToTableRows (ingredients: Ingredient list) =
        List.map ingredientToTableRow ingredients
        |> List.rev


    let ingredientRows = ingredientsToTableRows ingredients;

    Html.div [

        //Html.form [
          //  prop.onSubmit handleSubmit
            //prop.children [
        Html.label [
            Html.text "Name "
            Html.input [
                prop.type' "text"
                prop.value recipeName
                prop.onChange setRecipeName
            ]
        ]

        Html.div [
            prop.style[ style.marginTop 20 ]
            prop.children [
                Html.text "Ingredients:"
            ]
        ]

        Bulma.table [
            Html.thead [
                Html.tr [
                    Html.th [ prop.text "Qty" ]
                    Html.th [ prop.text "Ingredient" ]
                    Html.th []
                ]
            ]
            Html.tbody [
                yield! ingredientRows
                Html.tr [
                    Html.td [
                        Html.input [
                            prop.type' "number"
                            //prop.step 0.1
                            prop.style [ style.width 50 ]
                            prop.value (if quantity = 0.0 then "" else (string quantity))
                            //prop.onChange (fun (v: string) -> JS.console.log (parseFloatDefaultZero v))
                            prop.onChange (fun (v: string) -> parseFloatDefaultZero v |> setQuantity)
                            //prop.onInput (fun (ev) -> JS.console.log ev.target?value ) ***NEEDS Fable.Core.JSInterop
                        ]
                    ]
                    Html.td [
                        Html.input [
                            prop.type' "text"
                            prop.value name
                            prop.onChange setName
                        ]
                    ]
                    Html.td [
                        Html.button [
                            //prop.type' "submit"
                            prop.text "Add"
                            prop.onClick (fun _ -> saveIngredient())
                        ]
                    ]
                ]
            ]
        ]
        Html.div [
            Html.button [
                prop.text "Save";
                prop.style [ style.marginTop 20 ]
                prop.onClick (fun _ -> onSave { Name = recipeName; Ingredients = ingredients })
            ]
        ]
           // ]
      //  ]
    ]

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view (model: Model) dispatch =
    Html.div [
        prop.style [ style.backgroundColor "#eeeeee57"; style.minHeight (length.vh 100) ]
        prop.children [
            card "Counter Test" [
                Html.text ( sprintf "Counter: %i" model.Count )
                Html.div [
                    Html.button [ prop.onClick (fun _ -> dispatch Inc); prop.text "Inc" ]
                    Html.button [ prop.onClick (fun _ -> dispatch Dec); prop.text "Dec" ]
                    Html.button [ prop.onClick (fun _ -> dispatch Reset); prop.text "Reset" ]
                ]
            ]
            card "Add a recipe" [
                recipeBuilder (SaveRecipe >> dispatch)
            ]
        ]
    ]