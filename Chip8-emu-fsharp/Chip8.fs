module Chip8

open System

type Memory = array<uint8>
type Opcode = uint16
type FrameType = Drawable | Computational

// Memory layout
// 0x000-0x1FF - Chip 8 interpreter (contains font set in emu)
// 0x050-0x0A0 - Used for the built in 4x5 pixel font set (0-F)
// 0x200-0xFFF - Program ROM and work RAM
type State = { 
    Memory: Memory;  // 4096
    V: array<uint8>; // 16
    pc: uint16;
    I: uint16;
    gfx: array<bool>; // 2048
    delayTimer:uint8;
    soundTimer: uint8;
    stack: array<uint16>; // 16
    sp: uint16;
    terminating: bool * string;
}

type Command =
| SetIndex of uint16
// | ReturnFromSubroutine
| ClearScreen
| Unknown of uint16


type Frame = State * FrameType

type KeyInput =
| NormalPlay of uint8 []
| Pause
| Rewind
| Exit

let FetchFromMemory state (address: uint16 ) =
    state.Memory.[int32(address)]
    
let FetchOpcode state :Opcode =
    let leftPart = uint16(FetchFromMemory state state.pc)
    let rightPart = uint16(FetchFromMemory state (state.pc + 1us))
    (leftPart <<< 8) ||| rightPart

let DecodeNestedOpCode (opcode: Opcode) =
    match opcode &&& 0x00FFus with
    | 0x00E0us  -> ClearScreen
    //| 0x00EEus  -> ReturnFromSubroutine
    | _         -> Unknown opcode

let DecodeOpCode (opcode: Opcode) =
    match opcode &&& 0xF000us with
    | 0xA000us -> SetIndex (opcode &&& 0x0FFFus)
    | _        -> DecodeNestedOpCode opcode

let ExecuteCommand state command =
    match command with
    | SetIndex idx      -> { state with I = idx ; pc = state.pc + 2us }, Computational
    | ClearScreen       -> { state with gfx = (Array.create 2048 false) }, Drawable
    | Unknown opcode    -> { state with terminating = true, sprintf "Terminating because of unknown opcode %X" opcode }, Computational

let UpdateTimers frame =
    let state, frameType = frame
    { state with delayTimer = Math.Max(0uy, state.delayTimer - 1uy) ; soundTimer = Math.Max(0uy, state.soundTimer - 1uy) }, frameType    

let EmulateCycle state keys =
    FetchOpcode state
        |> DecodeOpCode
        |> ExecuteCommand state
        |> UpdateTimers

