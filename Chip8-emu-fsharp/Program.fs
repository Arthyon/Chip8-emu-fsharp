module Program

open Chip8
open Initialization

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
    let emptyInput = Array.create 16 0uy
    match DecodeOpCode emptyInput opcode with
    | Unknown code  -> if code <> 0x000us // Predetermined termination opcode during dev
                       then Some (sprintf "Program contains unhandled opcode %X" code)
                       else None
    | _             -> ValidateRom program (pos + 2)
                
let InitEmu bytes =
    let initialState = Initialization.Initialize bytes
    let initialInput = Initialization.initialInput

    initialState |> StepGameLoop [] initialInput