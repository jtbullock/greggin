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
    | Editing

type Model =
    {
        RecipeForm_Name: string
        RecipeForm_Ingredients: Ingredient list
        RecipeForm_NewIngredientQuantity: double
        RecipeForm_NewIngredientName: string

        Recipes: Recipe array
        EditingStatus: EditingStatus
    }

    static member Init = {
        RecipeForm_Name = ""
        RecipeForm_Ingredients = List.empty<Ingredient>
        RecipeForm_NewIngredientQuantity = 0.0
        RecipeForm_NewIngredientName = ""

        Recipes = Array.empty
        EditingStatus = NotEditing
    }

// ***************
// Update

type Msg =
    | Error of exn
    | RecipeSaved of Recipe array
    | SaveRecipe
    | RecipesLoaded of Recipe array
    | CreateRecipe
    | CancelCreateRecipe
    | RecipeForm_SaveIngredient
    | RecipeForm_DeleteIngredient of string
    | RecipeForm_Reset
    | RecipeForm_SetRecipeName of string
    | RecipeForm_SetQuantity of double
    | RecipeForm_SetIngredientName of string
    | EditRecipe of Recipe

let dojoApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IDojoApi>

let init () =
    Model.Init, Cmd.OfAsync.either dojoApi.GetRecipes () RecipesLoaded Error

let getRecipeFormRecipe (model) = {
    Name = model.RecipeForm_Name
    Ingredients = model.RecipeForm_Ingredients
}

let update msg model =
    match msg with
    | Error _ -> model, Cmd.none
    | RecipeSaved r ->
        { model with
            EditingStatus = NotEditing
            Recipes = r
        },
        Cmd.ofMsg RecipeForm_Reset
    | SaveRecipe -> model, Cmd.OfAsync.either dojoApi.PostRecipe (getRecipeFormRecipe model) RecipeSaved Error
    | RecipesLoaded r -> { model with Recipes = r }, Cmd.none
    | CreateRecipe -> { model with EditingStatus = Editing }, Cmd.none
    | CancelCreateRecipe ->
        { model with
            EditingStatus = NotEditing
        },
        Cmd.ofMsg RecipeForm_Reset
    | RecipeForm_SaveIngredient ->
        { model with
            RecipeForm_NewIngredientName = ""
            RecipeForm_NewIngredientQuantity = 0.0
            RecipeForm_Ingredients =
                {
                    Amount = model.RecipeForm_NewIngredientQuantity
                    Name = model.RecipeForm_NewIngredientName
                }
                :: model.RecipeForm_Ingredients
        },
        Cmd.none
    | RecipeForm_DeleteIngredient ingredientName ->
        { model with
            RecipeForm_Ingredients =
                model.RecipeForm_Ingredients
                |> List.filter (fun (i) -> i.Name <> ingredientName)
        },
        Cmd.none
    | RecipeForm_Reset ->
        { model with
            RecipeForm_Ingredients = List.empty<Ingredient>
            RecipeForm_Name = ""
            RecipeForm_NewIngredientQuantity = 0.0
            RecipeForm_NewIngredientName = ""
        },
        Cmd.none
    | RecipeForm_SetRecipeName name -> { model with RecipeForm_Name = name }, Cmd.none
    | RecipeForm_SetQuantity qty ->
        { model with
            RecipeForm_NewIngredientQuantity = qty
        },
        Cmd.none
    | RecipeForm_SetIngredientName name ->
        { model with
            RecipeForm_NewIngredientName = name
        },
        Cmd.none
    | EditRecipe recipe ->
        { model with
            EditingStatus = Editing
            RecipeForm_Name = recipe.Name
            RecipeForm_Ingredients = recipe.Ingredients
        },
        Cmd.none

// ***************
// View

let card (title: string) (content: ReactElement list) =
    Html.div [
        prop.style [ style.width 600; style.paddingTop 20 ]
        prop.children [ Bulma.box [ prop.children [ Bulma.subtitle title; yield! content ] ] ]
    ]

let parseFloat s =
    try
        Some(float s)
    with _ ->
        None

let parseFloatDefaultZero s =
    match parseFloat s with
    | Some f -> f
    | None -> 0.0

let recipeBuilder model dispatch =
    //let handleSubmit (e: Event) = do
    //    e.preventDefault()
    //    JS.console.log "Form submitted"

    let ingredientToTableRow (ingredient: Ingredient) =
        Html.tr [
            Html.td [ prop.text ingredient.Amount ]
            Html.td [ prop.text ingredient.Name ]
            Html.td [
                Html.button [
                    prop.type' "button"
                    prop.children [ Html.i [ prop.className "fas fa-trash" ] ]
                    prop.onClick (fun _ -> RecipeForm_DeleteIngredient ingredient.Name |> dispatch)
                ]
            ]
        ]

    let ingredientsToTableRows (ingredients: Ingredient list) =
        List.map ingredientToTableRow ingredients |> List.rev

    let ingredientRows = ingredientsToTableRows model.RecipeForm_Ingredients

    Html.div [

        //Html.form [
        //  prop.onSubmit handleSubmit
        //prop.children [
        Html.label [
            Html.text "Name "
            Html.input [
                prop.type' "text"
                prop.value model.RecipeForm_Name
                prop.onChange (fun name -> RecipeForm_SetRecipeName name |> dispatch)
                prop.autoComplete "off"
                prop.name "recipe-name"
            ]
        ]

        Html.div [ prop.style[style.marginTop 20]; prop.children [ Html.text "Ingredients:" ] ]

        Bulma.table [
            Html.thead [
                Html.tr [ Html.th [ prop.text "Qty" ]; Html.th [ prop.text "Ingredient" ]; Html.th [] ]
            ]
            Html.tbody [
                yield! ingredientRows
                Html.tr [
                    Html.td [
                        Html.input [
                            prop.type' "number"
                            //prop.step 0.1
                            prop.style [ style.width 50 ]
                            prop.value (
                                if model.RecipeForm_NewIngredientQuantity = 0.0 then
                                    ""
                                else
                                    (string model.RecipeForm_NewIngredientQuantity)
                            )
                            //prop.onChange (fun (v: string) -> JS.console.log (parseFloatDefaultZero v))
                            prop.onChange (fun (v: string) ->
                                parseFloatDefaultZero v |> RecipeForm_SetQuantity |> dispatch)
                        //prop.onInput (fun (ev) -> JS.console.log ev.target?value ) ***NEEDS Fable.Core.JSInterop
                        ]
                    ]
                    Html.td [
                        Html.input [
                            prop.type' "text"
                            prop.value model.RecipeForm_NewIngredientName
                            prop.onChange (fun name -> RecipeForm_SetIngredientName name |> dispatch)
                        ]
                    ]
                    Html.td [
                        Html.button [
                            //prop.type' "submit"
                            prop.text "Add"
                            prop.onClick (fun _ -> dispatch RecipeForm_SaveIngredient)
                        ]
                    ]
                ]
            ]
        ]
        Html.div [
            Html.button [
                prop.text "Save"
                prop.style [ style.marginTop 20 ]
                prop.onClick (fun _ -> dispatch SaveRecipe)
            ]
        ]
    // ]
    //  ]
    ]

let recipeRow dispatch recipe =
    Html.div [
        prop.style [
            style.marginTop 8
            style.border (1, borderStyle.solid, "#dbdbdb")
            style.borderRadius 4
            style.padding (7, 16)
            style.display.flex
            style.alignItems.center
        ]
        prop.children [
            Html.div [ prop.style [ style.flexGrow 1 ]; prop.children [ Html.text recipe.Name ] ]
            Bulma.button.a [
                Bulma.button.isInverted
                Bulma.color.isInfo
                prop.onClick (fun _ -> recipe |> EditRecipe |> dispatch)
                prop.children [ Html.i [ prop.className "fas fa-edit" ] ]
            ]
        ]
    ]

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view (model: Model) dispatch =
    Html.div [
        prop.style [ style.backgroundColor "#eeeeee57"; style.minHeight (length.vh 100) ]
        prop.children [
            Html.div [
                prop.style [ style.display.flex ]
                prop.children [
                    card "Recipes" [
                        Html.div [
                            Bulma.button.a [
                                prop.children [
                                    Html.i [ prop.className "fas fa-plus"; prop.style [ style.marginRight 5 ] ]
                                    Html.text "Add New Recipe"
                                ]

                                prop.onClick (fun _ -> (dispatch CreateRecipe))
                            ]
                        ]
                        yield! Array.map (recipeRow dispatch) model.Recipes
                    ]

                    card "Josh's Test" [ Html.text "Hello!" ]
                ]
            ]
            Bulma.modal [
                prop.id "modal-sample"
                if model.EditingStatus = Editing then
                    Bulma.modal.isActive
                prop.children [
                    Bulma.modalBackground []
                    Bulma.modalContent [ Bulma.box [ Html.h1 "Add a recipe"; recipeBuilder model dispatch ] ]
                    Bulma.modalClose [ prop.onClick (fun _ -> (dispatch CancelCreateRecipe)) ]
                ]
            ]
        ]
    ]