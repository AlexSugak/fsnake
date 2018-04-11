module App

open System
open Elmish
open Elmish.Browser.Navigation
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable

importAll "../sass/main.sass"

// MODEL
let WorldSize = 20 // number of cells per x and y axis
let Scale = 20 // number of px per cell
let RefreshRate = 200 // ms to tick the world state

type Position = {
    x : int
    y : int
}

type Direction =
    | Left
    | Right
    | Up
    | Down

type Msg =
    | StartGame
    | Turn of Direction
    | Tick

type State = {
    Snake : Position list
    Food: Position
    Moving : Direction
}

type Game =
    | NotStarted
    | InProgress of State
    | Over of State
    | Win of State

let init _ = NotStarted, []

// UPDATE
let newSnake = [{x=2; y=0}; {x=1; y=0}; {x=0; y=0}]
let newField snake = Array.ofSeq (seq {for i = 0 to WorldSize - 1 do
                                        for j = 0 to WorldSize - 1 do
                                            let p = {x=i;y=j}
                                            if List.contains p snake |> not then
                                                yield p })

let newFood (field: Position array) =
    let rnd = new Random()
    let next = rnd.Next(0, field.Length - 1)
    field.[next]

let mutable interval = 0.
let tick dispatch =
    interval <- window.setInterval((fun _ -> dispatch Tick), RefreshRate)

let stop _ =
    window.clearInterval(interval)

let move p d = match d with
                | Left -> {x = p.x - 1; y = p.y}
                | Right -> {x = p.x + 1; y = p.y}
                | Up -> {x = p.x; y = p.y - 1}
                | Down -> {x = p.x; y = p.y + 1}

let update msg model =
    match msg with
    | StartGame -> match model with
                    | Over _
                    | NotStarted -> let field = newField newSnake
                                    InProgress({Snake = newSnake
                                                Moving = Right
                                                Food = newFood field})
                    | _ -> model
                   , Cmd.ofSub tick

    | Tick -> match model with
                | InProgress s ->
                    let head = List.head s.Snake
                    let tailHead = s.Snake |> List.rev |> List.head
                    match move head s.Moving, tailHead with
                        | h', _ when 0 > h'.x
                                || h'.x >= WorldSize
                                || 0 > h'.y
                                || h'.y >= WorldSize
                                || List.contains h' s.Snake
                                -> Over s, Cmd.ofSub stop
                        | h', t when t = s.Food
                                ->  let snake = [h'] @ s.Snake
                                    let free = newField snake
                                    { s with Snake = snake
                                             Food = newFood free}
                                    |> InProgress, []
                        | h', _
                                ->  { s with Snake =
                                                List.rev s.Snake
                                                |> function
                                                    | [] -> []
                                                    | _::tail -> [h'] @ (List.rev tail) }
                                    |> InProgress, []
                | _ -> model, []

    | Turn d -> match model with
                | InProgress s -> InProgress({ s with Moving = d }), []
                | _ -> model, []

// VIEW
let [<Literal>] LEFT_KEY = 37.
let [<Literal>] UP_KEY = 38.
let [<Literal>] RIGHT_KEY = 39.
let [<Literal>] DOWN_KEY = 40.

let onGameKeyDown _ =
    let sub dispatch =
        (fun (e: KeyboardEvent) ->
            match e with
                | ev when ev.keyCode = UP_KEY -> dispatch (Turn Up)
                | ev when ev.keyCode = DOWN_KEY -> dispatch (Turn Down)
                | ev when ev.keyCode = LEFT_KEY -> dispatch (Turn Left)
                | ev when ev.keyCode = RIGHT_KEY -> dispatch (Turn Right)
                | _ -> ()
            :> obj)
        |> window.addEventListener_keydown
    Cmd.ofSub sub

let width n = unbox ("width", n)
let height n = unbox ("height", n)
let fill c = unbox ("fill", c)
let stroke c = unbox ("stroke", c)
let strokeWidth n = unbox ("strokeWidth", n)

let x n = unbox ("x", n)
let y n = unbox ("y", n)

let point color p =
    rect
        [width Scale
         height Scale
         fill color
         stroke "white"
         strokeWidth 2
         x (p.x * Scale)
         y (p.y * Scale)]
        []

let root model dispatch =
    let size = WorldSize * Scale
    div
        [Style [Padding 10]]
        [div
            []
            [svg
                [width size
                 height size]
                (   // the world border
                    [(rect
                        [width size
                         height size
                         fill "white"
                         stroke "black"
                         strokeWidth 1]
                        [])]
                    @
                    // food
                    (match model with
                        | InProgress s
                        | Over s
                        | Win s -> [point "green" s.Food]
                        | _ -> [])
                    @
                    // snake
                    (match model with
                        | InProgress s
                        | Over s
                        | Win s -> s.Snake |> List.map (point "black")
                        | _ -> [])
                )
            ]
         div
            []
            (match model with
                | NotStarted -> [button
                                    [OnClick (fun _ -> StartGame |> dispatch)]
                                    [str "Start Game"]]
                | Over _ -> [label [] [str "Game Over !"
                                       div
                                        []
                                        [button
                                            [OnClick (fun _ -> StartGame |> dispatch)]
                                            [str "Restart"]]]]
                | _ -> [])]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.withSubscription onGameKeyDown
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
