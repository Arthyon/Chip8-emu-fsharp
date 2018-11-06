module ``Decode opcode tests`` 

open Chip8
open Xunit
open FsUnit.Xunit

let emptyKeys = Array.create 16 0uy

[<Fact>]
let ``0x00E0 clears screen`` () = 
    DecodeOpCode emptyKeys 0x00E0us |> should equal ClearScreen

[<Fact>]
let ``0x00EE returns from subroutine`` () = 
    DecodeOpCode emptyKeys 0x00EEus |> should equal ReturnFromSubroutine

[<Fact>]
let ``0x1NNN jumps to address NNN`` () = 
    DecodeOpCode emptyKeys 0x15EEus |> should equal (Jump 0x5EEus)

[<Fact>]
let ``0x2NNN calls subroutine at NNN`` () = 
    DecodeOpCode emptyKeys 0x24EEus |> should equal (JumpToSubroutine 0x4EEus)

[<Fact>]
let ``0x3XNN skips if X is equal to NN`` () = 
    DecodeOpCode emptyKeys 0x3AEEus |> should equal (SkipIfTrue (10, 0xEEuy))

[<Fact>]
let ``0x4XNN skips if X is not equal to NN`` () = 
    DecodeOpCode emptyKeys 0x4AEEus |> should equal (SkipIfFalse (10, 0xEEuy))

[<Fact>]
let ``0x5XY0 skips if X is not equal to NN`` () = 
    DecodeOpCode emptyKeys 0x5AE0us |> should equal (SkipIfRegisterEq (10, 14))

[<Fact>]
let ``0x6XNN Sets VX to NN`` () = 
    DecodeOpCode emptyKeys 0x6AE0us |> should equal (SetRegister (10, 0xE0uy))

[<Fact>]
let ``0x7XNN Adds NN to VX without carry`` () = 
    DecodeOpCode emptyKeys 0x7EE0us |> should equal (AddNoCarry (14, 0xE0uy))

[<Fact>]
let ``0x8XY0 Assigns VX to VY`` () = 
    DecodeOpCode emptyKeys 0x8E50us |> should equal (Assign (14, 5))

[<Fact>]
let ``0x8XY1 Assigns VX to VX or VY`` () = 
    DecodeOpCode emptyKeys 0x8E51us |> should equal (BitOr (14, 5))

[<Fact>]
let ``0x8XY2 Assigns VX to VX and VY`` () = 
    DecodeOpCode emptyKeys 0x8E52us |> should equal (BitAnd (14, 5))

[<Fact>]
let ``0x8XY3 Assigns VX to VX xor VY`` () = 
    DecodeOpCode emptyKeys 0x8E53us |> should equal (BitXor (14, 5))
    
[<Fact>]
let ``0x8XY4 Adds VY to VX`` () = 
    DecodeOpCode emptyKeys 0x8E54us |> should equal (Add (14, 5))

[<Fact>]
let ``0x8XY5 Subtracts VY from VX`` () = 
    DecodeOpCode emptyKeys 0x8E55us |> should equal (Subtract (14, 5))

[<Fact>]
let ``0x8XY6 Bitshifts VX right, Stores LSB in VF`` () = 
    DecodeOpCode emptyKeys 0x8E56us |> should equal (BitShiftRight (14))

[<Fact>]
let ``0x8XY7 Subtracts VX from VY, stores in VX, Sets VF on borrow`` () = 
    DecodeOpCode emptyKeys 0x8E57us |> should equal (SubtractFromY (14, 5))

[<Fact>]
let ``0x8XYE Bitshifts VX left, stores in VX, Stores MSB in VF`` () = 
    DecodeOpCode emptyKeys 0x8E5Eus |> should equal (BitShiftLeft 14)

[<Fact>]
let ``0x9XY0 Skips next instruction if VX is not equal to VY`` () = 
    DecodeOpCode emptyKeys 0x9E50us |> should equal (SkipIfRegisterNotEq (14, 5))

[<Fact>]
let ``0xANNN Sets I to NNN`` () = 
    DecodeOpCode emptyKeys 0xAE50us |> should equal (SetIndex (0xE50us))

[<Fact>]
let ``0xBNNN Jumps to NNN plus V0`` () = 
    DecodeOpCode emptyKeys 0xBE50us |> should equal (JumpRelative (0xE50us))

[<Fact>]
let ``0xCXNN Sets VX to random number`` () = 
    DecodeOpCode emptyKeys 0xCE50us |> should equal (Rand (14, 0x50uy))

[<Fact>]
let ``0xDXYN Draws a sprite to the screen`` () = 
    DecodeOpCode emptyKeys 0xDE58us |> should equal (DrawSprite (14,5, 8))

[<Fact>]
let ``0xEX9E Skips next instruction if key is pressed`` () = 
    DecodeOpCode emptyKeys 0xED9Eus |> should equal (KeyPressed (13, emptyKeys))

[<Fact>]
let ``0xEXA1 Skips next instruction if key is not pressed`` () = 
    DecodeOpCode emptyKeys 0xEDA1us |> should equal (KeyNotPressed (13, emptyKeys))

[<Fact>]
let ``0xFX07 Sets VX to delay timer value`` () = 
    DecodeOpCode emptyKeys 0xFD07us |> should equal (GetTimer 13)

[<Fact>]
let ``0xFX0A Keypress is awaited`` () = 
    DecodeOpCode emptyKeys 0xF60Aus |> should equal (KeyPressBlocking (6, emptyKeys))

[<Fact>]
let ``0xFX15 Sets delay timer to VX`` () = 
    DecodeOpCode emptyKeys 0xFD15us |> should equal (SetTimer 13)

[<Fact>]
let ``0xFX18 Sets the sound timer to VX`` () = 
    DecodeOpCode emptyKeys 0xFD18us |> should equal (SetSound 13)

[<Fact>]
let ``0xFX1E Adds VX to I`` () = 
    DecodeOpCode emptyKeys 0xFB1Eus |> should equal (AddToIndex 11)

[<Fact>]
let ``0xFX29 Sets I to the location of the sprite for the character in VX`` () = 
    DecodeOpCode emptyKeys 0xFB29us |> should equal (MoveToSprite 11)

[<Fact>]
let ``0xFX33 Stores the binary coded representation of VX in memory`` () = 
    DecodeOpCode emptyKeys 0xFD33us |> should equal (BinaryCode 13)

[<Fact>]
let ``0xFX55 Stores V0 to VX in memory starting at address I`` () = 
    DecodeOpCode emptyKeys 0xF555us |> should equal (RegDump 5)

[<Fact>]
let ``0xFX65 Fills V0 to VX with memory values starting at address I`` () = 
    DecodeOpCode emptyKeys 0xF565us |> should equal (RegLoad 5)

[<Fact>]
let ``0x0NNN Is ignored`` () = 
    DecodeOpCode emptyKeys 0x0565us |> should equal IgnoredOpcode
