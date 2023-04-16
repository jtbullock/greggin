module Index

open Elmish

open Feliz
open Feliz.Bulma
open Feliz.Recharts
open Feliz.PigeonMaps
open Elmish.SweetAlert

open Fable.Remoting.Client
open Shared

/// The overall data model driving the view.
type Model = {
    Count: int
}

type Msg =
    | Inc
    | Dec
    | Reset

/// The init function is called to start the message pump with an initial view.
let init () =
    {
        Count = 0
    },
    Cmd.none

/// The update function knows how to update the model given a message.
let update msg model =
    match msg with
    | Inc -> { Count = model.Count + 1 }, Cmd.none
    | Dec -> { Count = model.Count - 1 }, Cmd.none
    | Reset -> init ()

let card (title:string) (content: ReactElement list) =
    Html.div [
        prop.style [ style.width 600; style.paddingTop 20; style.margin (0, length.auto)]
        prop.children [
            Bulma.box [ prop.children [ Bulma.subtitle title; yield! content ] ]
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
            card "Thing 2" [ (Html.text "Another thing") ]
        ]
    ]