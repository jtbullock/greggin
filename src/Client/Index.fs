module Index

open Elmish
open Fable.FontAwesome.Free
open Feliz
open Feliz.Bulma
open Fable.Remoting.Client
open Feliz.Styles
open Shared

// *************
// Model

type LeftColumnActivity =
    | ManageRecipes
    | RecipeBuilder
    | ConfirmDelete of string
    | PrepReport

type Model =
    {
        RecipeForm_Name: string
        RecipeForm_Ingredients: Ingredient list
        RecipeForm_NewIngredientQuantity: double
        RecipeForm_NewIngredientName: string

        Recipes: Recipe array
        RecipeSearchTerm: string
        LeftColumnActivity: LeftColumnActivity
        SelectedRecipes: Ingredient list
        Report: Report option
        CompletedRecipes: string list
    }

    static member Init = {
        RecipeForm_Name = ""
        RecipeForm_Ingredients = []
        RecipeForm_NewIngredientQuantity = 0.0
        RecipeForm_NewIngredientName = ""

        Recipes = Array.empty
        RecipeSearchTerm = ""
        LeftColumnActivity = ManageRecipes
        SelectedRecipes = []
        Report = None
        CompletedRecipes = []
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
    | SelectRecipe of string
    | DeselectRecipe of string
    | NavigateToReportPrep
    | CancelReportPrep
    | SetSelectionQuantity of (string * double)
    | ResetRecipeSelection
    | RunReport
    | ReportFinished of Report
    | RecipeCompleted of string
    | RecipeUncompleted of string
    | ResetCompleted

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
            LeftColumnActivity = ManageRecipes
            Recipes = r
        },
        Cmd.ofMsg RecipeForm_Reset
    | SaveRecipe -> model, Cmd.OfAsync.either dojoApi.PostRecipe (getRecipeFormRecipe model) RecipeSaved Error
    | RecipesLoaded r -> { model with Recipes = r }, Cmd.none
    | CreateRecipe ->
        { model with
            LeftColumnActivity = RecipeBuilder
        },
        Cmd.none
    | CancelCreateRecipe ->
        { model with
            LeftColumnActivity = ManageRecipes
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
            LeftColumnActivity = RecipeBuilder
            RecipeForm_Name = recipe.Name
            RecipeForm_Ingredients = recipe.Ingredients
        },
        Cmd.none
    | DeleteRecipe recipeName -> model, Cmd.OfAsync.either dojoApi.DeleteRecipe recipeName RecipeDeleted Error
    | RecipeDeleted recipes ->
        { model with
            Recipes = recipes
            LeftColumnActivity = ManageRecipes
        },
        Cmd.none
    | ConfirmDeleteRecipe recipe ->
        { model with
            LeftColumnActivity = ConfirmDelete recipe
        },
        Cmd.none
    | CancelDelete ->
        { model with
            LeftColumnActivity = ManageRecipes
        },
        Cmd.none
    | SetSearchTerm s -> { model with RecipeSearchTerm = s }, Cmd.none
    | SelectRecipe name ->
        { model with
            SelectedRecipes = { Amount = 1.0; Name = name } :: model.SelectedRecipes
        },
        Cmd.none
    | DeselectRecipe name ->
        let updatedRecipeSelections =
            List.filter (fun (r: Ingredient) -> r.Name <> name) model.SelectedRecipes

        { model with
            SelectedRecipes = updatedRecipeSelections
        },
        if updatedRecipeSelections.Length > 0 then
            Cmd.none
        else
            Cmd.ofMsg CancelReportPrep
    | NavigateToReportPrep ->
        { model with
            LeftColumnActivity = PrepReport
        },
        Cmd.none
    | CancelReportPrep ->
        { model with
            LeftColumnActivity = ManageRecipes
        },
        Cmd.none
    | SetSelectionQuantity (name, amt) ->
        { model with
            SelectedRecipes =
                List.map
                    (fun (r: Ingredient) -> if r.Name = name then { r with Amount = amt } else r)
                    model.SelectedRecipes
        },
        Cmd.none
    | ResetRecipeSelection ->
        { model with
            SelectedRecipes = List.empty<Ingredient>
        },
        Cmd.none
    | RunReport -> model, Cmd.OfAsync.either dojoApi.GetReport model.SelectedRecipes ReportFinished Error
    | ReportFinished report -> { model with Report = Some(report) }, Cmd.none
    | RecipeCompleted recipeName -> { model with CompletedRecipes = (recipeName :: model.CompletedRecipes) }, Cmd.none
    | RecipeUncompleted recipeName -> { model with CompletedRecipes = (List.filter (fun r -> r <> recipeName) model.CompletedRecipes) }, Cmd.none
    | ResetCompleted -> { model with CompletedRecipes = [] }, Cmd.none

// ***************
// View

let column (content: ReactElement list) =
    Html.div [
        prop.style [
            style.minWidth 400
            style.display.flex
            style.flexDirection.column
            style.overflow.hidden
            style.padding (20, 20, 0, 20)
            style.backgroundColor color.white
            style.boxShadow (4, 0, 4, color.lightGray)
            style.marginRight 10
        ]
        prop.children content
    ]

let icon iconName =
    Html.i [ prop.className (sprintf "fas %s" iconName) ]

let iconButton iconName (text: string) onClick =
    Bulma.button.button [
        prop.children [
            Html.i [ prop.style[style.marginRight 10]; prop.className (sprintf "fas %s" iconName) ]
            Html.text text
        ]
        prop.onClick onClick
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

let recipeBuilder dispatch model =
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
        Bulma.subtitle "Recipe"

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

let recipeRow dispatch (recipe: (bool * Recipe)) =

    let (isChecked, recipe) = recipe

    let handleCheckboxChange isChecked =
        recipe.Name
        |> if isChecked = true then SelectRecipe else DeselectRecipe
        |> dispatch

    Html.div [
        prop.style [
            style.borderBottom (1, borderStyle.solid, "#dbdbdb")
            style.padding (7, 7, 7, 16)
            style.display.flex
            style.alignItems.center
        ]
        prop.children [
            // Recipe name and checkbox
            Html.label [
                prop.style [ style.flexGrow 1; style.cursor.pointer ]
                prop.children [
                    Bulma.input.checkbox [
                        prop.isChecked isChecked
                        prop.value "remember"
                        prop.style [ style.marginRight 10 ]
                        prop.onChange handleCheckboxChange
                    ]
                    Bulma.text.span recipe.Name
                ]
            ]

            // Recipe actions
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

let reportIngredientRow dispatch (ingredient: Ingredient) =

    let onAmountChange (amt: string) =
        let parsedAmount = parseFloatDefaultZero amt
        SetSelectionQuantity(ingredient.Name, parsedAmount) |> dispatch

    Html.div [
        prop.style [
            style.borderBottom (1, borderStyle.solid, "#dbdbdb")
            style.padding (7, 7, 7, 16)
            style.display.flex
            style.alignItems.center
        ]
        prop.children [
            // Recipe name and amount
            Bulma.input.number [
                prop.style [ style.width 80; style.marginRight 10 ]
                prop.value ingredient.Amount
                prop.onChange onAmountChange
            ]

            Html.div [ prop.style [ style.flexGrow 1 ]; prop.children [ Html.text ingredient.Name ] ]

            // Recipe actions
            Bulma.button.a [
                Bulma.button.isInverted
                Bulma.color.isInfo
                prop.onClick (fun _ -> ingredient.Name |> DeselectRecipe |> dispatch)
                prop.children [ Html.i [ prop.className "fas fa-trash" ] ]
            ]
        ]
    ]

let reportSetup dispatch (recipes: Ingredient list) =
    React.fragment [
        Bulma.subtitle "Configure Report"
        iconButton "fa-caret-left" "Return to Recipes" (fun _ -> (dispatch CancelReportPrep))

        Html.div [
            prop.style [
                style.overflowY.auto
                style.border (1, borderStyle.solid, color.rgb (219, 219, 219))
                style.flexGrow 1
                style.margin (20, 0)
                style.borderRadius 4
            ]
            prop.children (List.map (fun r -> reportIngredientRow dispatch r) recipes)
        ]

        Html.div [
            prop.style [style.marginBottom 20; style.textAlign.center]
            prop.children [
                iconButton "fa-play" "Run Report" (fun _ -> (dispatch RunReport))
            ]
        ]

    ]

let recipeList dispatch (recipes: (bool * Recipe) array) (searchTerm: string) =

    let applySearchTermToRecipe (recipeName: string) =
        let lowerCase = recipeName.ToLower()
        lowerCase.Contains(searchTerm.ToLower())

    Html.div [
        prop.style [
            style.overflowY.auto
            style.border (1, borderStyle.solid, color.rgb (219, 219, 219))
            style.flexGrow 1
            style.marginTop 20
            style.borderRadius 4
        ]
        prop.children (
            recipes
            |> Array.filter (fun r -> applySearchTermToRecipe (snd r).Name)
            |> Array.map (recipeRow dispatch)
        )
    ]

let pageContainer (content: ReactElement list) =
    Html.div [
        prop.style [ style.display.flex; style.height (length.vh 100); style.overflowX.auto ]
        prop.children content
    ]

let recipeSearchBox dispatch (searchTerm: string) =
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

let manageRecipes dispatch model =
    let isRecipeSelected (recipe: Recipe) =
        let lookup =
            List.tryFind (fun (selectedItem: Ingredient) -> selectedItem.Name = recipe.Name) model.SelectedRecipes

        match lookup with
        | Some _ -> true
        | _ -> false

    let recipesWithSelection =
        Array.map (fun r -> ((isRecipeSelected r), r)) model.Recipes

    React.fragment [
        iconButton "fa-plus" "Add New Recipe" (fun _ -> (dispatch CreateRecipe))

        recipeSearchBox dispatch model.RecipeSearchTerm

        recipeList dispatch recipesWithSelection model.RecipeSearchTerm

        if model.SelectedRecipes.Length > 0 then
            Html.div [
                prop.style [ style.textAlign.center; style.padding (10, 0) ]
                prop.children[iconButton "fa-times" "Reset Selection" (fun _ -> (dispatch ResetRecipeSelection))
                              iconButton "fa-play" "Run Report" (fun _ -> (dispatch NavigateToReportPrep))]
            ]
        else
            Html.none
    ]

let deleteConfirmation dispatch (recipeName: string) =
    Html.div [
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

let printReportIngredientRow dispatch (completedRecipes:string list) (ingredient:Ingredient) =
    let isCompleted = (List.contains ingredient.Name completedRecipes)
    let textStyle = if isCompleted then style.textDecoration.lineThrough else style.textDecoration.none;
    Html.tr [
        Html.td [
            Bulma.input.checkbox [
                prop.isChecked isCompleted
                prop.onChange (fun (ckd:bool) -> ingredient.Name
                                                 |> (if ckd then RecipeCompleted else RecipeUncompleted)
                                                 |> dispatch)
            ]
        ]
        Html.td [
            prop.style [ textStyle ]
            prop.text ingredient.Amount
        ]
        Html.td [
            prop.style [textStyle]
            prop.text ingredient.Name
        ]
    ]

let printStage dispatch (completedRecipes: string list) (stage: CraftingStage) (title: string) =
    Html.div [
        prop.style [ style.minWidth 350; style.marginRight 40 ]
        prop.children [
            Html.div [
                prop.style [
                    style.textAlign.center
                    style.borderBottom (1, borderStyle.solid, color.lightGray)
                    style.marginBottom 10
                    style.paddingBottom 8
                ]
                prop.children ( Bulma.subtitle title )
            ]


            Html.div [
                prop.style [ style.overflowY.auto  ]
                prop.children [
                    Bulma.table [
                        prop.style [ style.width (length.percent 100) ]

                        prop.children [
                            Html.thead [
                                Html.tr [
                                    Html.th [prop.width 30]
                                    Html.th [prop.text "Amt"; prop.width 80]
                                    Html.th [prop.text "Ingredient"]
                                ]
                            ]
                            Html.tbody [
                                yield! List.map (printReportIngredientRow dispatch completedRecipes) stage.Ingredients
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let printReport dispatch (report: Report) (completedRecipes: string list) =
    Html.div [
        prop.style [style.display.flex; style.width (400 * report.Stages.Length)]
        prop.children (List.mapi (fun i s -> printStage dispatch completedRecipes s (if (i = report.Stages.Length - 1) then "Final products" else $"Stage %i{s.Stage + 1}") ) report.Stages)
    ]

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view (model: Model) dispatch =
    React.fragment [
        pageContainer [
            column [
                match model.LeftColumnActivity with
                | ManageRecipes -> manageRecipes dispatch model
                | RecipeBuilder -> recipeBuilder dispatch model
                | ConfirmDelete recipeName -> deleteConfirmation dispatch recipeName
                | PrepReport -> reportSetup dispatch model.SelectedRecipes
            ]

            match model.Report with
            | Some report ->
                Html.div [
                    prop.style [ style.padding 20 ]
                    prop.children [
                        Html.div [
                            prop.style [style.display.flex; style.alignItems.center; style.marginBottom 10]
                            prop.children [
                                Html.span [
                                    prop.style [ style.fontWeight 400; style.fontSize (length.rem 1.25); style.marginRight 20 ]
                                    prop.text "Report"
                                ]
                                Html.a [
                                    prop.href "#"
                                    prop.text "Uncheck All"
                                    prop.onClick (fun _ -> (dispatch ResetCompleted))
                                ]
                            ]
                        ]


                        printReport dispatch report model.CompletedRecipes
                    ]
                ]
            | None -> Html.none
        ]
    ]