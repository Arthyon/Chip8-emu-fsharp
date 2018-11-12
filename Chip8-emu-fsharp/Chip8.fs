module Chip8

open System
open OpcodeHandler

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
let toXYI opcode =
    toX opcode, toY opcode, int(opcode &&& 0x00Fus)

let FetchFromMemory state (address: uint16 ) =
    state.Memory.[int32(address)]
    
let FetchOpcode state :Opcode =
    let leftPart = uint16(FetchFromMemory state state.pc)
    let rightPart = uint16(FetchFromMemory state (state.pc + 1us))
    (leftPart <<< 8) ||| rightPart

let DecodeNestedOpCodeLsb (opcode: Opcode) =
    match opcode &&& 0xF00Fus with
    | 0x8000us  -> Assign (toXY opcode)
    | 0x8001us  -> BitOr (toXY opcode)
    | 0x8002us  -> BitAnd (toXY opcode)
    | 0x8003us  -> BitXor (toXY opcode)
    | 0x8004us  -> Add (toXY opcode)
    | 0x8005us  -> Subtract (toXY opcode)
    | 0x8006us  -> BitShiftRight (toX opcode)
    | 0x8007us  -> SubtractFromY (toXY opcode)
    | 0x800Eus  -> BitShiftLeft (toX opcode)
    | _         -> if (opcode &&& 0xF000us) = 0x0000us 
                   then IgnoredOpcode 
                   else Unknown opcode

let DecodeNestedOpCode (opcode: Opcode) keys =
    match opcode &&& 0xF0FFus with
    | 0x00E0us  -> ClearScreen
    | 0x00EEus  -> ReturnFromSubroutine
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
    | 0xD000us -> DrawSprite (toXYI opcode)
    | _        -> DecodeNestedOpCode opcode keys

let ExecuteCommand (state, stateMutator) logger command =
    sprintf "%A" command |> logger
    match command with
    | SetIndex idx                  -> hSetIndex idx >> incrementPc
    | Jump value                    -> hJump value
    | JumpToSubroutine value        -> hJumpToSubroutine value
    | SkipIfTrue (addr, value)      -> hSkipIfTrue addr value
    | SkipIfFalse (addr, value)     -> hSkipIfFalse addr value
    | SkipIfRegisterEq (x, y)       -> hSkipIfRegisterEquals x y
    | SetRegister (addr, value)     -> hSetRegister addr value >> incrementPc
    | AddNoCarry (addr, value)      -> hAddNoCarry addr value >> incrementPc
    | ReturnFromSubroutine          -> hReturnFromSubroutine >> incrementPc
    | ClearScreen                   -> hClearScreen >> incrementPc >> redraw
    | BinaryCode x                  -> handleBinaryCode x >> incrementPc
    | Add (x, y)                    -> handleAdd x y >> incrementPc
    | Assign (x, y)                 -> hAssign x y >> incrementPc
    | AddToIndex idx                -> hAddToIndex idx >> incrementPc
    | BitAnd (x , y)                -> hBitAnd x y >> incrementPc
    | BitOr (x , y)                 -> hBitOr x y >> incrementPc
    | BitShiftLeft x                -> hBitshiftLeft x >> incrementPc
    | BitShiftRight x               -> hBitshiftRight x >> incrementPc
    | BitXor (x, y)                 -> hBitXor x y >> incrementPc
    | GetTimer x                    -> hGetTimer x >> incrementPc
    | SetTimer x                    -> hSetTimer x >> incrementPc
    | JumpRelative N                -> hJumpRelative N
    | SetSound x                    -> hSetSound x >> incrementPc
    | SkipIfRegisterNotEq (x, y)    -> hSkipIfRegisterNotEq x y
    | Subtract (x,y)                -> hSubtract x y >> incrementPc
    | KeyPressed (x, keys)          -> hKeyPress x keys
    | KeyNotPressed (x, keys)       -> hKeyNotPressed x keys
    | SubtractFromY (x, y)          -> hSubtractFromY x y >> incrementPc
    | KeyPressBlocking (x, keys)    -> hKeyPressBlocking x keys
    | DrawSprite (x, y, height)     -> hDrawSprite x y height >> incrementPc >> redraw
    | MoveToSprite x                -> hMoveToSprite x >> incrementPc
    | Rand (x, n)                   -> hRand x n >> incrementPc
    | RegDump x                     -> hRegDump x >> incrementPc
    | RegLoad x                     -> hRegLoad x >> incrementPc
    | IgnoredOpcode                 -> incrementPc    
    | Unknown opcode                -> fun (s,m) -> { s with terminating = true, sprintf "Terminating because of unknown opcode %X" opcode }, m
    <| (state, stateMutator)

let ResetFrameType state (stateMutator: StateMutator) =
    stateMutator.frameTypeMutator <- fun _ -> FrameType.Computational
    state, stateMutator

let UpdateTimers (state, (stateMutator: StateMutator)) =
    stateMutator.delayTimerMutator <- stateMutator.delayTimerMutator >> (fun t -> Math.Max(0uy, t - 1uy))
    stateMutator.soundTimerMutator <- stateMutator.soundTimerMutator >> (fun t -> Math.Max(0uy, state.soundTimer - 1uy))
    state, stateMutator


let UpdateState (state, (stateMutator: StateMutator))  =
    { 
        Memory = stateMutator.MemoryMutator state.Memory ;
        V = stateMutator.VMutator state.V ;
        pc = stateMutator.pcMutator state.pc;
        I = stateMutator.IMutator state.I;
        gfx = stateMutator.gfxMutator state.gfx;
        delayTimer = stateMutator.delayTimerMutator state.delayTimer;
        soundTimer = stateMutator.soundTimerMutator state.soundTimer;
        stack = stateMutator.stackMutator state.stack;
        sp = stateMutator.spMutator state.sp;
        frameType = stateMutator.frameTypeMutator state.frameType;
        terminating = state.terminating
    }, stateMutator


let EmulateCycle state keys logger (stateMutator: StateMutator) =
    stateMutator.Reset()
    let stateTup = stateMutator |> ResetFrameType state
    state 
        |> FetchOpcode 
        |> DecodeOpCode keys
        |> ExecuteCommand stateTup logger
        |> UpdateTimers
        |> UpdateState

