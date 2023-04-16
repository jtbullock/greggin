module Index

open Elmish

open Feliz
open Feliz.Bulma
open Feliz.Recharts
open Feliz.PigeonMaps
open Elmish.SweetAlert

open Fable.Remoting.Client
open Shared
open Fable.Core

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

/// The init function is called to start the message pump with an initial view.
let init () =
    {
        Count = 0
        Ingredients = List.empty
    },
    Cmd.none

/// The update function knows how to update the model given a message.
let update msg model =
    match msg with
    | Inc -> { model with Count = model.Count + 1 }, Cmd.none
    | Dec -> { model with Count = model.Count - 1 }, Cmd.none
    | IngredientAdd i -> { model with Ingredients = i :: model.Ingredients }, Cmd.none
    | Reset -> init ()

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
let recipeBuilder() =

    let (ingredients, setIngredients) = React.useState(List.empty<double * string>)

    let (quantity, setQuantity) = React.useState(2.5)
    let (name, setName) = React.useState("")

    let saveIngredient() =
        setIngredients ((quantity, name) :: ingredients)
        setQuantity 0.0
        setName ""

    Html.div [
        Html.text "Ingredients:"

        Html.div [
            yield! List.map (fun (qty, name: string) -> Html.text (sprintf "%f %s" qty name)) ingredients 
        ]

        Html.div [
            Html.input [
                prop.type' "number"
                //prop.step 0.1
                prop.style [ style.width 50 ]
                prop.value quantity
                //prop.onChange (fun (v: string) -> JS.console.log (parseFloatDefaultZero v))
                prop.onChange (fun (v: string) -> parseFloatDefaultZero v |> setQuantity)
            ]
            Html.input [
                prop.type' "text"
                prop.value name
                prop.onChange setName
            ]
            Html.button [
                prop.text "Add"
                prop.onClick (fun _ -> saveIngredient())
            ]
        ]
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
            card "Thing 2" [
                Html.label [
                    Html.text "Name "
                    Html.input [
                        prop.type' "text"
                    ]
                ]
                recipeBuilder ()
                Html.div [
                    Html.button [ prop.text "Save" ]
                ]
            ]
        ]
    ]