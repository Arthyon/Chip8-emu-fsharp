open Chip8
open Initialization
open System.IO
open System


let TryPlaySound state =
    if state.soundTimer = 1uy 
    then printfn "Beep"

let Draw state =
    ()

let GetCurrentInput previousInput =
    previousInput

let NextFrame state keys =
    let newState, frameType = EmulateCycle state keys
    match frameType with
    | Computational -> TryPlaySound newState
                       newState
    | Drawable      -> TryPlaySound newState
                       Draw newState
                       newState

let rec RunGameLoop previousInput (previousStates: State list) state =
    if fst state.terminating 
    then state, previousStates
    else
        let input = GetCurrentInput previousInput
        match input with
        | NormalPlay keys   ->  NextFrame state keys
                                |> RunGameLoop input (state::previousStates)
        | Pause             ->  RunGameLoop input previousStates state
        | Rewind            ->  match previousStates with
                                | head::tail    ->  RunGameLoop input tail head
                                | []            ->  RunGameLoop input previousStates state
        | Exit              ->  state, previousStates
                
[<EntryPoint>]
let main argv =
    if argv.Length = 0 && not (File.Exists argv.[0]) then failwith "Invalid path"

    let game = File.ReadAllBytes argv.[0]
    let initialState = Initialization.Initialize game

    let endstate, _ = initialState |> RunGameLoop Initialization.initialInput []

    let _, terminationReason = endstate.terminating
    printf "%s" terminationReason
    printf "Last state of I: %X" endstate.I
    0 
