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

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view (model: Model) dispatch =
    Html.div [
        prop.style [ style.backgroundColor "#eeeeee57"; style.minHeight (length.vh 100) ]
        prop.children [
            Html.h1 "Hello!"
            Html.text ( sprintf "Counter: %i" model.Count )
            Html.button [ prop.onClick (fun _ -> dispatch Inc); prop.text "Inc" ]
            Html.button [ prop.onClick (fun _ -> dispatch Dec); prop.text "Dec" ]
        ]
    ]