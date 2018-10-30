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
    let state = { initialState with gfx = [|true;true;false|] }
    ExecuteCommand state ClearScreen
    |> should equal { state with gfx = (Array.create 2048 false) ; pc = state.pc + 2us; frameType = Drawable }

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

    
