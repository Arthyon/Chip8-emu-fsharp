module Program
open Chip8
open Initialization
open System.IO
open System


let TryPlaySound state =
    if state.soundTimer = 1uy 
    then printfn "Beep"

let NextFrame state keys sendState =
    let newState, frameType = EmulateCycle state keys
    match frameType with
    | Computational -> TryPlaySound newState
                       newState
    | Drawable      -> TryPlaySound newState
                       sendState newState
                       newState

let rec RunGameLoop previousInput (previousStates: State list) getCurrentInput sendState state =
    if fst state.terminating 
    then state, previousStates
    else
        let input = getCurrentInput previousInput
        match input with
        | NormalPlay keys   ->  NextFrame state keys sendState
                                |> RunGameLoop input (state::previousStates) getCurrentInput sendState
        | Pause             ->  RunGameLoop input previousStates getCurrentInput sendState state
        | Rewind            ->  match previousStates with
                                | head::tail    ->  RunGameLoop input tail getCurrentInput sendState head
                                | []            ->  RunGameLoop input previousStates getCurrentInput sendState state
        | Exit              ->  state, previousStates

let rec ValidateRom (program: byte[]) pos =
    if pos > program.Length - 2 
    then None
    else

    let leftPart = uint16(program.[pos])
    let rightPart = uint16(program.[pos + 1])
    let opcode = (leftPart <<< 8) ||| rightPart
    match DecodeOpCode opcode with
    | Unknown code  -> if code <> 0x000us // Predetermined termination opcode during dev
                       then Some (sprintf "Program contains unhandled opcode %X" code)
                       else None
    | _             -> ValidateRom program (pos + 2)
                
let Run bytes getInput sendGfx =
    let initialState = Initialization.Initialize bytes

    initialState |> RunGameLoop Initialization.initialInput [] getInput sendGfx