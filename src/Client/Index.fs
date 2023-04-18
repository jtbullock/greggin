module Index

open Elmish
open Feliz
open Feliz.Bulma
open Fable.Remoting.Client
open Shared

// *************
// Model

type EditingStatus =
    | NotEditing
    | New
    | Editing of string

type Model = {
    Count: int
    Ingredients: Ingredient list
    Recipes: Recipe array
    EditingStatus: EditingStatus
}

// ***************
// Update

type Msg =
    | Error of exn
    | RecipeSaved of Recipe array
    | SaveRecipe of Recipe
    | RecipesLoaded of Recipe array
    | CreateRecipe
    | CancelCreateRecipe

let dojoApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IDojoApi>

let init () =
    {
        Count = 0
        Ingredients = List.empty
        Recipes = Array.empty
        EditingStatus = NotEditing
    },
    Cmd.OfAsync.either dojoApi.GetRecipes () RecipesLoaded Error
    
let update msg model =
    match msg with
    | Error _ -> model, Cmd.none
    | RecipeSaved r -> { model with EditingStatus = NotEditing; Recipes = r }, Cmd.none
    | SaveRecipe r -> model, Cmd.OfAsync.either dojoApi.PostRecipe r RecipeSaved Error
    | RecipesLoaded r -> { model with Recipes = r }, Cmd.none
    | CreateRecipe -> { model with EditingStatus = New }, Cmd.none
    | CancelCreateRecipe -> { model with EditingStatus = NotEditing }, Cmd.none

// ***************
// View

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

    let reset() =
        setIngredients(List.empty<Ingredient>)
        setRecipeName("")
        setQuantity(0.0)
        setName("")

    //let handleSubmit (e: Event) = do
    //    e.preventDefault()
    //    JS.console.log "Form submitted"

    // There's some READ BAD juju going on here:
    //let saveRecipe =
    //    onSave { Name = recipeName; Ingredients = ingredients }
    // onSave is called every time this re-renders
    // I wonder why? 🤔

    let saveRecipe () =
        reset()
        onSave { Name = recipeName; Ingredients = ingredients }

    let ingredientToTableRow (ingredient: Ingredient) =
        Html.tr [
            Html.td [ prop.text ingredient.Amount ]
            Html.td [ prop.text ingredient.Name ]
            Html.td [
                Html.button [
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
                prop.autoComplete "off"
                prop.name "recipe-name"
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
                prop.onClick (fun _ -> saveRecipe())
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
            card "Recipes" [
                Html.div [
                    Bulma.button.a [
                        prop.text "Add new recipe"
                        prop.onClick (fun _ -> (dispatch CreateRecipe))
                    ]
                ]
                yield! Array.map (fun r -> Html.div [ Html.text r.Name ] ) model.Recipes
            ]
            Bulma.modal [
                prop.id "modal-sample"
                if model.EditingStatus = New then Bulma.modal.isActive
                prop.children [
                    Bulma.modalBackground []
                    Bulma.modalContent [
                        Bulma.box [
                            Html.h1 "Add a recipe"
                            recipeBuilder (SaveRecipe >> dispatch)
                        ]
                    ]
                    Bulma.modalClose [ prop.onClick (fun _ -> (dispatch CancelCreateRecipe)) ]
                ]
            ]
        ]
    ]