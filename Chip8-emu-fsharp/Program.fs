module Program

open Chip8
open Initialization

let StepGameLoop (previousStates: State list) input logger mutator state =
    match input with
    | NormalPlay keys   ->  let newState, mutator = EmulateCycle state keys logger mutator
                            match newState.frameType with
                            | FrameType.Computational   -> previousStates, newState, mutator
                            | FrameType.Drawable        -> (state::previousStates), newState, mutator
    | Rewind            ->  match previousStates with
                            | head::tail    ->  tail, head, mutator
                            | []            ->  previousStates, state, mutator

let InitEmu bytes logger mutator =
    let initialState = Initialization.Initialize bytes
    let initialInput = Initialization.initialInput

    initialState |> StepGameLoop [] initialInput logger mutator