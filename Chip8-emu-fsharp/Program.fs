module Program

open Chip8
open Initialization

let StepGameLoop (previousStates: State list) input logger state =
    match input with
    | NormalPlay keys   ->  let newState = EmulateCycle state keys logger
                            (state::previousStates), newState
    | Rewind            ->  match previousStates with
                            | head::tail    ->  tail, head
                            | []            ->  previousStates, state

let InitEmu bytes logger =
    let initialState = Initialization.Initialize bytes
    let initialInput = Initialization.initialInput

    initialState |> StepGameLoop [] initialInput logger