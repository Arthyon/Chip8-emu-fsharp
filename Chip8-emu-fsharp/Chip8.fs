module Chip8

open System
open OpcodeHandler

// TODO Check if bitshift is correct here
let toY opcode =
    int32((opcode &&& 0x00F0us) >>> 4)
let toX opcode =
    int32((opcode &&& 0x0F00us) >>> 8)
let toN opcode =
    uint8(opcode &&& 0x000Fus)
let toNN opcode =
    uint8(opcode &&& 0x00FFus)
let toNNN opcode =
    uint16(opcode &&& 0x0FFFus)

let toXY opcode =
    toX opcode, toY opcode
let toXYN opcode =
    toX opcode, toY opcode, toN opcode
let toXNN opcode =
    toX opcode, toNN opcode

let FetchFromMemory state (address: uint16 ) =
    state.Memory.[int32(address)]
    
let FetchOpcode state :Opcode =
    let leftPart = uint16(FetchFromMemory state state.pc)
    let rightPart = uint16(FetchFromMemory state (state.pc + 1us))
    (leftPart <<< 8) ||| rightPart

let DecodeNestedOpCodeLsb (opcode: Opcode) =
    match opcode &&& 0xF00Fus with
    | 0x0000us  -> ClearScreen
    | 0x000Eus  -> ReturnFromSubroutine
    | 0x8000us  -> Assign (toXY opcode)
    | 0x8001us  -> BitOr (toXY opcode)
    | 0x8002us  -> BitAnd (toXY opcode)
    | 0x8003us  -> BitXor (toXY opcode)
    | 0x8004us  -> Add (toXY opcode)
    | 0x8005us  -> Subtract (toXY opcode)
    | 0x8006us  -> BitShiftRight (toX opcode)
    | 0x8007us  -> SubtractFromY (toXY opcode)
    | 0x800Eus  -> BitShiftLeft (toX opcode)
    | _         -> Unknown opcode

let DecodeNestedOpCode (opcode: Opcode) keys =
    match opcode &&& 0xF0FFus with
    | 0xE09Eus  -> KeyPressed ((toX opcode) , keys) 
    | 0xE0A1us  -> KeyNotPressed ((toX opcode), keys)
    | 0xF007us  -> GetTimer (toX opcode)
    | 0xF00Aus  -> KeyPressBlocking (toX opcode, keys)
    | 0xF015us  -> SetTimer (toX opcode)
    | 0xF018us  -> SetSound (toX opcode)
    | 0xF01Eus  -> AddToIndex (toX opcode)
    | 0xF029us  -> MoveToSprite (toX opcode)
    | 0xF033us  -> BinaryCode (toX opcode)
    | 0xF055us  -> RegDump (toX opcode)
    | 0xF065us  -> RegLoad (toX opcode)
    | _         -> DecodeNestedOpCodeLsb opcode

let DecodeOpCode keys (opcode: Opcode) =
    match opcode &&& 0xF000us with
    | 0x1000us -> Jump (toNNN opcode)
    | 0x2000us -> JumpToSubroutine (toNNN opcode)
    | 0x3000us -> SkipIfTrue (toXNN opcode)
    | 0x4000us -> SkipIfFalse (toXNN opcode)
    | 0x5000us -> SkipIfRegisterEq (toXY opcode)
    | 0x6000us -> SetRegister (toXNN opcode)
    | 0x7000us -> AddNoCarry (toXNN opcode)
    | 0x9000us -> SkipIfRegisterNotEq (toXY opcode)
    | 0xA000us -> SetIndex (toNNN opcode)
    | 0xB000us -> JumpRelative (toNNN opcode)
    | 0xC000us -> Rand (toXNN opcode)
    | 0xD000us -> DrawSprite (toXYN opcode)
    | _        -> DecodeNestedOpCode opcode keys

let ExecuteCommand state command =
    match command with
    | SetIndex idx                  -> (fun s -> { s with I = idx }) >> incrementPc
    | Jump value                    -> (fun s ->  { s with pc = value })
    | JumpToSubroutine value        -> mutateStack >> hJumpToSubroutine value
    | SkipIfTrue (addr, value)      -> hSkipIfTrue addr value
    | SkipIfFalse (addr, value)     -> hSkipIfFalse addr value
    | SkipIfRegisterEq (x, y)       -> hSkipIfRegisterEquals x y
    | SetRegister (addr, value)     -> mutateRegister >> hSetRegister addr value >> incrementPc
    | AddNoCarry (addr, value)      -> mutateRegister >> hAddNoCarry addr value >> incrementPc
    | ReturnFromSubroutine          -> hReturnFromSubroutine >> incrementPc
    | ClearScreen                   -> (fun s ->  { s with gfx = (Array.create 2048 false); } ) >> incrementPc >> redraw
    | BinaryCode x                  -> mutateMemory >> handleBinaryCode x >> incrementPc
    | Add (x, y)                    -> mutateRegister >> handleAdd x y >> incrementPc
    | Assign (x, y)                 -> mutateRegister >> hAssign x y >> incrementPc
    | AddToIndex idx                -> (fun s -> { s with I = s.I + uint16(s.V.[idx]) }) >> incrementPc
    | BitAnd (x , y)                -> mutateRegister >> hBitAnd x y >> incrementPc
    | BitOr (x , y)                 -> mutateRegister >> hBitOr x y >> incrementPc
    | BitShiftLeft x                -> mutateRegister >> hBitshiftLeft x >> incrementPc
    | BitShiftRight x               -> mutateRegister >> hBitshiftRight x >> incrementPc
    | BitXor (x, y)                 -> mutateRegister >> hBitXor x y >> incrementPc
    | GetTimer x                    -> mutateRegister >> hGetTimer x >> incrementPc
    | SetTimer x                    -> mutateRegister >> (fun s -> { s with delayTimer = s.V.[x] }) >> incrementPc
    | JumpRelative N                -> (fun s -> { s with pc = uint16(s.V.[0]) + N })
    | SetSound x                    -> (fun s -> { s with soundTimer = s.V.[x] }) >> incrementPc
    | SkipIfRegisterNotEq (x, y)    -> hSkipIfRegisterNotEq x y
    | Subtract (x,y)                -> mutateRegister >> hSubtract x y >> incrementPc
    | KeyPressed (x, keys)          -> hKeyPress x keys
    | KeyNotPressed (x, keys)       -> hKeyNotPressed x keys
    | SubtractFromY (x, y)          -> mutateRegister >> hSubtractFromY x y >> incrementPc
    | KeyPressBlocking (x, keys)    -> mutateRegister >> hKeyPressBlocking x keys
    | Unknown opcode                -> fun s -> { s with terminating = true, sprintf "Terminating because of unknown opcode %X" opcode }
    <| state

let ResetFrameType state =
    { state with frameType = Computational }
let UpdateTimers state =
    { state with delayTimer = Math.Max(0uy, state.delayTimer - 1uy) ; soundTimer = Math.Max(0uy, state.soundTimer - 1uy) }

let EmulateCycle state keys =
    FetchOpcode state
        |> DecodeOpCode keys
        |> ExecuteCommand (ResetFrameType state)
        |> UpdateTimers

