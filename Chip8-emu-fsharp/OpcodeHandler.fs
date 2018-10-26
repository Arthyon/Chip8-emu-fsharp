module OpcodeHandler
let redraw state =
    { state with frameType = Drawable }

let incrementPc state =
    { state with pc = state.pc + 2us }

let mutateRegister state =
    let reg = Array.copy state.V
    { state with V = reg }

let mutateStack state =
    let stack = Array.copy state.stack
    { state with stack = stack }

let mutateMemory state =
    let mem = Array.copy state.Memory
    { state with Memory = mem }

let hJumpToSubroutine value state =
    state.stack.[int32(state.sp)] <- state.pc
    { state with sp = state.sp + 1us ; pc = value }

let handleAdd x y state =
    let xvalue = state.V.[x]
    let yvalue = state.V.[y]
    let carry = if (uint16(xvalue) + uint16(yvalue)) > 0xFFus then 1uy else 0uy
    state.V.[x] <- (xvalue + yvalue)
    state.V.[0xF] <-carry
    state

let handleBinaryCode x state =
    let xvalue = state.V.[x]
    let moved = xvalue >>> 8
    state.Memory.[int32(state.I)] <- (moved / 100uy)
    state.Memory.[int32(state.I + 1us)] <- (moved / 10uy % 10uy)
    state.Memory.[int32(state.I + 2us)] <- (moved % 100uy % 10uy)
    state

let hReturnFromSubroutine state =
    let pointer = state.sp - 1us
    { state with sp = pointer ; pc = state.stack.[int32(pointer)] }

let hSkipIfTrue addr value state =
    let pc =    if state.V.[addr] = value 
                then state.pc + 4us 
                else state.pc + 2us
    { state with pc = pc }

let hSkipIfFalse addr value state =
    let pc =    if state.V.[addr] <> value 
                then state.pc + 4us 
                else state.pc + 2us
    { state with pc = pc }

let hSkipIfRegisterEquals x y state =
    let pc =    if state.V.[x] = state.V.[y]
                then state.pc + 4us
                else state.pc + 2us
    { state with pc = pc }

let hSetRegister addr value state =
    state.V.[addr] <- value
    state

let hAddNoCarry addr value state =
    state.V.[addr] <- state.V.[addr] + value
    state

let hAssign x y state =
    state.V.[x] <- state.V.[y]
    state
