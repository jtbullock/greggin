module Index

open Elmish
open Feliz
open Feliz.Bulma
open Fable.Remoting.Client
open Shared

// *************
// Model

type ModalStatus =
    | Open
    | Closed

type DeleteModalStatus =
    | Confirm of string
    | Closed

type Model =
    {
        RecipeForm_Name: string
        RecipeForm_Ingredients: Ingredient list
        RecipeForm_NewIngredientQuantity: double
        RecipeForm_NewIngredientName: string

        Recipes: Recipe array
        EditingModalStatus: ModalStatus
        DeleteConfirmModalStatus: DeleteModalStatus
        RecipeSearchTerm: string
    }

    static member Init = {
        RecipeForm_Name = ""
        RecipeForm_Ingredients = List.empty<Ingredient>
        RecipeForm_NewIngredientQuantity = 0.0
        RecipeForm_NewIngredientName = ""

        Recipes = Array.empty
        EditingModalStatus = ModalStatus.Closed
        DeleteConfirmModalStatus = Closed
        RecipeSearchTerm = ""
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
    | DeleteRecipe of string
    | RecipeDeleted of Recipe array
    | ConfirmDeleteRecipe of string
    | CancelDelete
    | SetSearchTerm of string

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
            EditingModalStatus = ModalStatus.Closed
            Recipes = r
        },
        Cmd.ofMsg RecipeForm_Reset
    | SaveRecipe -> model, Cmd.OfAsync.either dojoApi.PostRecipe (getRecipeFormRecipe model) RecipeSaved Error
    | RecipesLoaded r -> { model with Recipes = r }, Cmd.none
    | CreateRecipe -> { model with EditingModalStatus = Open }, Cmd.none
    | CancelCreateRecipe ->
        { model with
            EditingModalStatus = ModalStatus.Closed
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
            EditingModalStatus = Open
            RecipeForm_Name = recipe.Name
            RecipeForm_Ingredients = recipe.Ingredients
        },
        Cmd.none
    | DeleteRecipe recipeName -> model, Cmd.OfAsync.either dojoApi.DeleteRecipe recipeName RecipeDeleted Error
    | RecipeDeleted recipes ->
        { model with
            Recipes = recipes
            DeleteConfirmModalStatus = Closed
        },
        Cmd.none
    | ConfirmDeleteRecipe recipe ->
        { model with
            DeleteConfirmModalStatus = Confirm recipe
        },
        Cmd.none
    | CancelDelete ->
        { model with
            DeleteConfirmModalStatus = Closed
        },
        Cmd.none
    | SetSearchTerm s -> { model with RecipeSearchTerm = s }, Cmd.none

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
            Bulma.button.button [
                Bulma.color.isPrimary
                prop.text "Save"
                prop.style [ style.marginTop 20 ]
                prop.onClick (fun _ -> dispatch SaveRecipe)
            ]
            Bulma.button.button [
                Bulma.color.isInfo
                Bulma.button.isInverted
                prop.text "Cancel"
                prop.style [ style.marginTop 20 ]
                prop.onClick (fun _ -> dispatch CancelCreateRecipe)
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
            style.padding (7, 7, 7, 16)
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
            Bulma.button.a [
                Bulma.button.isInverted
                Bulma.color.isInfo
                prop.onClick (fun _ -> recipe.Name |> ConfirmDeleteRecipe |> dispatch)
                prop.children [ Html.i [ prop.className "fas fa-trash" ] ]
            ]
        ]
    ]

let deleteConfirmModal dispatch (modalStatus: DeleteModalStatus) =
    match modalStatus with
    | Closed -> Html.text "" // How to return nothing?
    | Confirm recipeName ->
        Bulma.modal [
            Bulma.modal.isActive
            prop.children [
                Bulma.modalBackground []
                Bulma.modalContent [
                    Bulma.box [
                        Bulma.subtitle "Delete Confirmation"
                        Html.text "Are you sure you want to delete recipe '"
                        Html.strong recipeName
                        Html.text "'?"

                        Html.div [
                            prop.style [ style.marginTop 20 ]
                            prop.children [
                                Bulma.button.button [
                                    prop.text "Delete"
                                    Bulma.color.isDanger
                                    prop.onClick (fun _ -> recipeName |> DeleteRecipe |> dispatch)
                                ]
                                Bulma.button.button [
                                    prop.text "Cancel"
                                    Bulma.button.isInverted
                                    Bulma.color.isInfo
                                    prop.onClick (fun _ -> dispatch CancelDelete)
                                ]
                            ]
                        ]
                    ]
                ]
                Bulma.modalClose [ prop.onClick (fun _ -> (dispatch CancelDelete)) ]
            ]
        ]

let editRecipeModal dispatch model =
    Bulma.modal [
        prop.id "edit-modal"
        if model.EditingModalStatus = Open then
            Bulma.modal.isActive
        prop.children [
            Bulma.modalBackground []
            Bulma.modalContent [ Bulma.box [ Bulma.subtitle "Add a recipe"; recipeBuilder model dispatch ] ]
            Bulma.modalClose [ prop.onClick (fun _ -> (dispatch CancelCreateRecipe)) ]
        ]
    ]

let recipeList dispatch recipes (searchTerm: string) =

    let applySearchTermToRecipe (recipeName: string) =
        let lowerCase = recipeName.ToLower()
        lowerCase.Contains(searchTerm.ToLower())

    card "Recipes" [
        Html.div [
            Bulma.button.a [
                prop.children [
                    Html.i [ prop.className "fas fa-plus"; prop.style [ style.marginRight 5 ] ]
                    Html.text "Add New Recipe"
                ]

                prop.onClick (fun _ -> (dispatch CreateRecipe))
            ]
            Html.div [
                prop.style [ style.display.flex; style.marginTop 20 ]

                prop.children [
                    Bulma.input.text [
                        prop.style [ style.flexGrow 1 ]
                        prop.placeholder "search"
                        prop.value searchTerm
                        prop.onChange (fun t -> t |> SetSearchTerm |> dispatch)
                    ]
                    Bulma.button.button [
                        Bulma.button.isInverted
                        Bulma.color.isInfo
                        prop.children [ Html.i [ prop.className "fas fa-times" ] ]
                        prop.onClick (fun _ -> dispatch (SetSearchTerm ""))
                    ]
                ]
            ]
        ]
        yield!
            recipes
            |> Array.filter (fun r -> applySearchTermToRecipe r.Name)
            |> Array.map (recipeRow dispatch)
    ]

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view (model: Model) dispatch =
    Html.div [
        prop.style [ style.backgroundColor "#eeeeee57"; style.minHeight (length.vh 100) ]
        prop.children [
            Html.div [
                prop.style [ style.display.flex ]
                prop.children [
                    recipeList dispatch model.Recipes model.RecipeSearchTerm
                    card "Josh's Test" [ Html.text "Hello!" ]
                ]
            ]
            editRecipeModal dispatch model
            deleteConfirmModal dispatch model.DeleteConfirmModalStatus
        ]
    ]