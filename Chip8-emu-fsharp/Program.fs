
open System
type Memory = array<uint8>
type State = { 
    Memory: Memory; 
    V: array<uint8>;
    pc: uint16;
    I: uint16;
    gfx: array<bool>;
    delayTimer:uint8;
    soundTimer: uint8;
    stack: array<uint16>;
    sp: uint16;
}

type Command =
| SetIndex of uint16

type Opcode = uint16

type Frame =
| DrawableFrame of State
| ComputationalFrame of State


// 0x000-0x1FF - Chip 8 interpreter (contains font set in emu)
// 0x050-0x0A0 - Used for the built in 4x5 pixel font set (0-F)
// 0x200-0xFFF - Program ROM and work RAM
let fontset = Array.create 80 0uy

let Initialize (programBytes : byte[])=
    let systemMemory = Array.create 4096 0uy
    for i = 0 to 79 do
        systemMemory.[i + 80] <- fontset.[i]
    for i = 0 to (programBytes.Length - 1) do
        systemMemory.[i + 512] <- programBytes.[i]

    {
        Memory = systemMemory
        V = (Array.create 16 0uy);
        pc = 0x200us;
        I = 0us;
        gfx = (Array.create 2048 false);
        delayTimer = 0uy;
        soundTimer = 0uy;
        stack = (Array.create 16 0us);
        sp = 0us;
    }

let FetchFromMemory state (address: uint16 ) =
    state.Memory.[int32(address)]
    
let FetchOpcode state :Opcode =
    let leftPart = uint16(FetchFromMemory state state.pc)
    let rightPart = uint16(FetchFromMemory state (state.pc + 1us))
    (leftPart <<< 8) ||| rightPart

let DecodeOpCode (opcode: Opcode )=
    match opcode &&& 0xF000us with
    | 0xA000us -> SetIndex (opcode &&& 0x0FFFus)
    | _ -> failwith "Unknown opcode"

let ExecuteCommand command state =
    match command with
    | SetIndex idx -> { state with I = idx ; pc = state.pc + 2us }

let EmulateCycle state =
    let opcode = FetchOpcode (state)
    let command = DecodeOpCode opcode
    let state = ExecuteCommand command state
    { state with delayTimer = Math.Max(0uy, state.delayTimer - 1uy) ; soundTimer = Math.Max(0uy, state.soundTimer - 1uy) }

let PlaySound () =
    printfn "Beep"

[<EntryPoint>]
let main argv =
    let game = [|0xAFuy ; 0xF2uy|]

    let state = Initialize game
    let newState = EmulateCycle state
    if newState.soundTimer = 1uy then PlaySound ()

    printfn "%X" newState.I
    0 
