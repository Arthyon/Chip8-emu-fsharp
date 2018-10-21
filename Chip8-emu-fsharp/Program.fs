module Program
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
                
let main file =
    if not (File.Exists file) then failwith "Invalid path"

    let game = File.ReadAllBytes file
    let validationResult = ValidateRom game 0
    if validationResult.IsSome 
    then 
        sprintf "%s" validationResult.Value
    else
        let initialState = Initialization.Initialize game

        let endstate, _ = initialState |> RunGameLoop Initialization.initialInput []

        let _, terminationReason = endstate.terminating
        printfn "%s" terminationReason
        sprintf "Last state of I: %X" endstate.I
