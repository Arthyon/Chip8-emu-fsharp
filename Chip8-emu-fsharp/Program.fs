module Program
open Chip8
open Initialization
open System.IO
open System


//let TryPlaySound state =
//    if state.soundTimer = 1uy 
//    then printfn "Beep"

//let NextFrame state keys sendState =
//    let newState, frameType = EmulateCycle state keys
//    match frameType with
//    | Computational -> TryPlaySound newState
//                       newState
//    | Drawable      -> TryPlaySound newState
//                       sendState newState
//                       newState

let StepGameLoop (previousStates: State list) input state =
    match input with
    | NormalPlay keys   ->  let newState = EmulateCycle state keys
                            (state::previousStates), newState
    | Rewind            ->  match previousStates with
                            | head::tail    ->  tail, head
                            | []            ->  previousStates, state

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
                
let InitEmu bytes =
    let initialState = Initialization.Initialize bytes
    let initialInput = Initialization.initialInput

    initialState |> StepGameLoop [] initialInput