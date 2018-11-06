module ``Execute command tests`` 

open Chip8
open Xunit
open FsUnit.Xunit
open OpcodeHandler

let initialState = Initialization.Initialization.Initialize [||]

[<Fact>]
let ``SetIndex. Sets I to val, increments pc`` () =
    ExecuteCommand initialState (SetIndex 0xF64us)
    |> should equal { initialState with I = 0xF64us ; pc = (initialState.pc + 2us)}

[<Fact>]
let ``Jump. Sets pc to val`` () =
    let state = ExecuteCommand initialState (Jump 0x23Fus)
    state |> should equal { initialState with pc = 0x23Fus }

[<Fact>]
let ``JumpToSubroutine. Stores pc on stack, sets pc to val`` () =
    let stack = Array.copy initialState.stack 
    stack.[int32(initialState.sp)] <- initialState.pc

    ExecuteCommand initialState (JumpToSubroutine 0x231us)
    |> should equal { initialState with pc = 0x231us ; sp = initialState.sp + 1us ; stack = stack }

[<Fact>]
let ``SkipIfTrue. Skips next instruction if true`` () =
    let state = { initialState with V = [|0uy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfTrue (1, 0xEEuy))
    |> should equal { state with pc = 0x204us}

[<Fact>]
let ``SkipIfTrue. Will not skip if not true`` () =
    let state = { initialState with V = [|0uy;0xEFuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfTrue (1, 0xEEuy))
    |> should equal { state with pc = 0x202us}

[<Fact>]
let ``SkipIfFalse. Will not skip next instruction if true`` () =
    let state = { initialState with V = [|0uy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfFalse (1, 0xEEuy))
    |> should equal { state with pc = 0x202us}

[<Fact>]
let ``SkipIfFalse. Skips next instruction if false`` () =
    let state = { initialState with V = [|0uy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfFalse (1, 0xEFuy))
    |> should equal { state with pc = 0x204us}

[<Fact>]
let ``SkipIfRegisterEq. Skips next instruction if VX is equal to VY`` () =
    let state = { initialState with V = [|0xEEuy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfRegisterEq (1, 0))
    |> should equal { state with pc = 0x204us}

[<Fact>]
let ``SkipIfRegisterEq. Does not skip next instruction if VX is not equal to VY`` () =
    let state = { initialState with V = [|0xBAuy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfRegisterEq (1, 0))
    |> should equal { state with pc = 0x202us}

[<Fact>]
let ``SetRegister. Sets VX to NN`` () =
    let state = mutateRegister initialState
    let newState = ExecuteCommand state (SetRegister (2, 0x43uy))
    newState.V.[2] |> should equal 0x43uy
    

[<Fact>]
let ``AddNoCarry. V0 is not set even on overflow`` () =

    let state = mutateRegister initialState
    state.V.[0] <- 0xFEuy

    let newState = ExecuteCommand state (AddNoCarry (0, 0x55uy))
    newState.V.[0] |> should equal 83uy
    newState.V.[0xF] |> should equal 0uy
    newState.pc |> should equal (state.pc + 2us)

[<Fact>]
let ``ReturnFromSubroutine. Sets pc to previous stack value, resets pointer, increments pc`` () =
    let state = { initialState with stack = [|0x200us;0x500us|]; sp = 1us }
    ExecuteCommand state ReturnFromSubroutine
    |> should equal { state with sp = 0us ; pc = 0x202us }

[<Fact>]
let ``ClearScreen. Resets gfx array, increments pc and marks frame as drawable`` () =
    let state = { initialState with gfx = [|1uy;1uy;0uy|] }
    ExecuteCommand state ClearScreen
    |> should equal { state with gfx = (Array.create 2048 0uy) ; pc = state.pc + 2us; frameType = Drawable }

[<Fact>]
let ``BinaryCode. Assigns binary coded representation of VX to memory`` () =
    let state = initialState |> mutateRegister
    state.V.[0] <- 0xAEuy
    let expectedState = { (mutateMemory state) with pc = state.pc + 2us }
    expectedState.Memory.[int32(state.I)]  <- 1uy
    expectedState.Memory.[int32(state.I + 1us)]  <- 7uy
    expectedState.Memory.[int32(state.I + 2us)]  <- 4uy

    ExecuteCommand state (BinaryCode 0)
    |> should equal expectedState

[<Fact>]
let ``Add. Adds VY to VX, VF set to 1 on carry`` () =
    
    let state = mutateRegister initialState
    state.V.[0] <- 0xFEuy
    state.V.[1] <- 0x55uy
    state.V.[0xF] <- 0uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 83uy
    expectedState.V.[0xF] <- 1uy
    
    ExecuteCommand state (Add (0,1))
    |> should equal expectedState


[<Fact>]
let ``Add. Adds VY to VX, VF set to 0 when no carry`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0x4uy
    state.V.[1] <- 0x55uy
    state.V.[0xF] <- 1uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0x59uy
    expectedState.V.[0xF] <- 0uy
    
    ExecuteCommand state (Add (0,1))
    |> should equal expectedState

[<Fact>]
let ``Assign. Sets VX to VY, increments pc`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0x45uy
    state.V.[4] <- 0x23uy
    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0x23uy
    ExecuteCommand state (Assign (0,4)) 
    |> should equal expectedState


[<Fact>]
let ``Unknown. Terminates application`` () =
    let state = ExecuteCommand initialState (Unknown 0x023Fus)
    state |> should equal { initialState with terminating = true, sprintf"Terminating because of unknown opcode %X" 0x023Fus}

[<Fact>]
let ``AddToIndex. Adds VX to I, increments pc`` () =
    let state = { (mutateRegister initialState) with I = 0x001us }
    let expectedState = { state with I = 0x004us ; pc = state.pc + 2us }
    state.V.[0] <- 0x003uy
    ExecuteCommand state (AddToIndex 0)
    |> should equal expectedState

[<Fact>]
let ``BitAnd. ANDs VX and VY, stores in VX and increments pc`` () =
    let state = mutateRegister initialState
    state.V.[2] <- 0xFEuy
    state.V.[5] <- 0xF7uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[2] <- 0xF6uy

    ExecuteCommand state (BitAnd (2, 5))
    |> should equal expectedState

[<Fact>]
let ``BitOr. ORs VX and VY, stores in VX and increments pc`` () =
    let state = mutateRegister initialState
    state.V.[2] <- 0xFEuy
    state.V.[5] <- 0xF7uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[2] <- 0xFFuy

    ExecuteCommand state (BitOr (2, 5))
    |> should equal expectedState

[<Fact>]
let ``BitshiftLeft. Stores MSB in VF, shifts VX left when MSB is 1`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0xEBuy
    state.V.[0xF] <- 0uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0xD6uy
    expectedState.V.[0xF] <- 1uy
    ExecuteCommand state (BitShiftLeft 0)
    |>should equal expectedState
    
[<Fact>]
let ``BitshiftLeft. Stores MSB in VF, shifts VX left when MSB is 0`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0x64uy
    state.V.[0xF] <- 1uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0xC8uy
    expectedState.V.[0xF] <- 0uy
    ExecuteCommand state (BitShiftLeft 0)
    |>should equal expectedState

[<Fact>]
let ``BitshiftRight. Stores LSB in VF, shifts VX right when LSB is 1`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0xEBuy
    state.V.[0xF] <- 0uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0x75uy
    expectedState.V.[0xF] <- 1uy
    ExecuteCommand state (BitShiftRight 0)
    |>should equal expectedState

[<Fact>]
let ``BitshiftRight. Stores LSB in VF, shifts VX right when LSB is 0`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0xECuy
    state.V.[0xF] <- 1uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0x76uy
    expectedState.V.[0xF] <- 0uy
    ExecuteCommand state (BitShiftRight 0)
    |> should equal expectedState

[<Fact>]
let ``BitXor. XORs VX and VY, stores in VX and increments pc`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0xE4uy
    state.V.[1] <- 0x37uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 0xD3uy
    ExecuteCommand state (BitXor (0, 1))
    |> should equal expectedState

[<Fact>]
let ``GetTimer. Sets VX to delaytimer, increments pc`` () =
    let state = { (mutateRegister initialState) with delayTimer = 3uy }

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[5] <- 3uy

    ExecuteCommand state (GetTimer 5)
    |> should equal expectedState
    

[<Fact>]
let ``SetTimer. Sets delaytimer to VX, increments pc`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 4uy

    let expectedState = {(mutateRegister state) with pc = state.pc + 2us ; delayTimer = 4uy }

    ExecuteCommand state (SetTimer 0)
    |> should equal expectedState

[<Fact>]
let ``JumpRelative. Sets pc to V0 + N`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 0x50uy

    ExecuteCommand state (JumpRelative 0x202us)
    |> should equal { state with pc = 0x252us }

[<Fact>]
let ``SetSound. Sets soundTimer to VX, increments pc`` () =
    let state = mutateRegister initialState
    state.V.[1] <- 5uy

    ExecuteCommand state (SetSound 1)
    |> should equal { state with soundTimer = 5uy; pc = state.pc + 2us }

[<Fact>]
let ``SkipIfRegisterNotEq. Skips next instruction if VX is not equal to VY`` () =
    let state = { initialState with V = [|0xBAuy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfRegisterNotEq (1, 0))
    |> should equal { state with pc = 0x204us}

[<Fact>]
let ``SkipIfRegisterNotEq. Does not skip next instruction if VX is equal to VY`` () =
    let state = { initialState with V = [|0xEEuy;0xEEuy|] ; pc = 0x200us }
    ExecuteCommand state (SkipIfRegisterNotEq (1, 0))
    |> should equal { state with pc = 0x202us} 

[<Fact>]
let ``Subtract. Subtracts VY from VX, VF set to 0 when no borrow`` () =
    
    let state = mutateRegister initialState
    state.V.[0] <- 254uy
    state.V.[1] <- 85uy
    state.V.[0xF] <- 1uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 169uy
    expectedState.V.[0xF] <- 0uy
    
    ExecuteCommand state (Subtract (0,1))
    |> should equal expectedState


[<Fact>]
let ``Subtract. Subtract VY from VX, VF set to 1 on borrow`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 4uy
    state.V.[1] <- 85uy
    state.V.[0xF] <- 0uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 175uy
    expectedState.V.[0xF] <- 1uy
    
    ExecuteCommand state (Subtract (0,1))
    |> should equal expectedState

[<Fact>]
let ``KeyPressed. Skips next instruction if key x is pressed`` () =
    let keys = Array.create 16 0uy
    keys.[3] <- 1uy
    ExecuteCommand initialState (KeyPressed (3, keys))
    |> should equal { initialState with pc = initialState.pc + 4us }

[<Fact>]
let ``KeyPressed. Does not skip nextinstruction if key x is not pressed`` () =
    let keys = Array.create 16 0uy
    keys.[3] <- 0uy
    ExecuteCommand initialState (KeyPressed (3, keys))
    |> should equal { initialState with pc = initialState.pc + 2us }
    
[<Fact>]
let ``KeyNotPressed. Skips next instruction if key x is not pressed`` () =
    let keys = Array.create 16 0uy
    keys.[3] <- 0uy
    ExecuteCommand initialState (KeyNotPressed (3, keys))
    |> should equal { initialState with pc = initialState.pc + 4us }

[<Fact>]
let ``KeyNotPressed. Does not skip next instruction if key x is pressed`` () =
    let keys = Array.create 16 0uy
    keys.[3] <- 1uy
    ExecuteCommand initialState (KeyNotPressed (3, keys))
    |> should equal { initialState with pc = initialState.pc + 2us }


[<Fact>]
let ``SubtractFromY. Subtracts VX from VY, stored in VX, VF set to 0 when no borrow`` () =
    
    let state = mutateRegister initialState
    state.V.[0] <- 85uy
    state.V.[1] <- 254uy
    state.V.[0xF] <- 1uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 169uy
    expectedState.V.[0xF] <- 0uy
    
    ExecuteCommand state (SubtractFromY (0, 1))
    |> should equal expectedState


[<Fact>]
let ``SubtractFromY. Subtract VX from VY, stored in VX,, VF set to 1 on borrow`` () =
    let state = mutateRegister initialState
    state.V.[0] <- 85uy
    state.V.[1] <- 4uy
    state.V.[0xF] <- 0uy

    let expectedState = { (mutateRegister state) with pc = state.pc + 2us }
    expectedState.V.[0] <- 175uy
    expectedState.V.[0xF] <- 1uy
    
    ExecuteCommand state (SubtractFromY (0, 1))
    |> should equal expectedState

[<Fact>]
let ``KeyPressBlocking. Will not increment pc when no keys are pressed`` () =
    let keys = Array.create 16 0uy
    ExecuteCommand initialState (KeyPressBlocking (2, keys))
    |> should equal initialState
    
[<Fact>]
let ``KeyPressBlocking. Stores first keypress idx in VX, increments pc`` () =
    let keys = Array.create 16 0uy
    keys.[2] <- 1uy
    keys.[6] <- 1uy
    let expectedState = { (mutateRegister initialState) with pc = initialState.pc + 2us }
    expectedState.V.[5] <- 2uy

    ExecuteCommand initialState (KeyPressBlocking (5, keys))
    |> should equal expectedState
    
[<Fact>]
let ``DrawSprite. Draws sprite to screen without flipping VF, increments pc`` () =
    let state = initialState |> mutateMemory
    state.Memory.[int(state.I)] <- 0x3Cuy
    state.Memory.[int(state.I) + 1] <- 0xC3uy
    state.Memory.[int(state.I) + 2] <- 0xFFuy

    let expectedState = { state with pc = state.pc + 2us; frameType = FrameType.Drawable } |> mutateGfx
    expectedState.gfx.[2] <- 1uy
    expectedState.gfx.[3] <- 1uy
    expectedState.gfx.[4] <- 1uy
    expectedState.gfx.[5] <- 1uy

    expectedState.gfx.[64] <- 1uy
    expectedState.gfx.[65] <- 1uy
    expectedState.gfx.[70] <- 1uy
    expectedState.gfx.[71] <- 1uy


    expectedState.gfx.[128] <- 1uy
    expectedState.gfx.[129] <- 1uy
    expectedState.gfx.[130] <- 1uy
    expectedState.gfx.[131] <- 1uy
    expectedState.gfx.[132] <- 1uy
    expectedState.gfx.[133] <- 1uy
    expectedState.gfx.[134] <- 1uy
    expectedState.gfx.[135] <- 1uy

    let returnedState = ExecuteCommand state (DrawSprite (0, 0, 3))
    returnedState |> should equal expectedState
    
[<Fact>]
let ``DrawSprite. Draws sprite to screen and flipping VF, increments pc`` () =
    let state = initialState |> mutateMemory |> mutateGfx
    state.Memory.[int(state.I)] <- 0x3Cuy
    state.Memory.[int(state.I) + 1] <- 0xC3uy
    state.Memory.[int(state.I) + 2] <- 0xFFuy
    state.gfx.[64] <- 1uy

    let expectedState = { state with pc = state.pc + 2us; frameType = FrameType.Drawable } |> mutateGfx |> mutateRegister
    expectedState.V.[0xF] <- 1uy

    expectedState.gfx.[2] <- 1uy
    expectedState.gfx.[3] <- 1uy
    expectedState.gfx.[4] <- 1uy
    expectedState.gfx.[5] <- 1uy

    expectedState.gfx.[64] <- 0uy
    expectedState.gfx.[65] <- 1uy
    expectedState.gfx.[70] <- 1uy
    expectedState.gfx.[71] <- 1uy


    expectedState.gfx.[128] <- 1uy
    expectedState.gfx.[129] <- 1uy
    expectedState.gfx.[130] <- 1uy
    expectedState.gfx.[131] <- 1uy
    expectedState.gfx.[132] <- 1uy
    expectedState.gfx.[133] <- 1uy
    expectedState.gfx.[134] <- 1uy
    expectedState.gfx.[135] <- 1uy

    let returnedState = ExecuteCommand state (DrawSprite (0, 0, 3))
    returnedState |> should equal expectedState
