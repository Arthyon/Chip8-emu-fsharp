[<AutoOpen>]
module Models

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
    gfx: array<uint8>; // 2048
    delayTimer:uint8;
    soundTimer: uint8;
    stack: array<uint16>; // 16
    sp: uint16;
    frameType: FrameType;
    terminating: bool * string;
}

type Command =
| JumpToSubroutine of uint16
| BinaryCode of int
| ReturnFromSubroutine
| Jump of uint16
| SkipIfTrue of int * uint8
| SkipIfFalse of int * uint8
| SkipIfRegisterEq of int * int
| SetRegister of int * uint8
| AddNoCarry of int * uint8
| Assign of int * int
| BitOr of int * int
| BitAnd of int * int
| BitXor of int * int
| Add of int * int
| Subtract of int * int
| BitShiftRight of int
| SubtractFromY of int * int
| BitShiftLeft of int
| SkipIfRegisterNotEq of int * int
| SetIndex of uint16
| JumpRelative of uint16
| Rand of int * uint8
| GetTimer of int
| SetTimer of int
| SetSound of int
| AddToIndex of int
| MoveToSprite of int
| RegDump of int
| RegLoad of int
| ClearScreen
| DrawSprite of int * int * int
| KeyPressed of int * uint8[]
| KeyNotPressed of int * uint8[]
| KeyPressBlocking of int * uint8[]
| IgnoredOpcode
| Unknown of uint16


type Frame = State * FrameType

type KeyInput =
| NormalPlay of uint8 []
| Rewind

