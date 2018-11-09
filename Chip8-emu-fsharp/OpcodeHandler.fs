module OpcodeHandler

let mutateArray mutator arr =
    let newArray = Array.copy arr
    mutator newArray
    newArray
    
    

let mutateRegister state =
    let reg = Array.copy state.V
    { state with V = reg }

let mutateGfx state =
    let gfx = Array.copy state.gfx
    { state with gfx = gfx }

let mutateStack state =
    let stack = Array.copy state.stack
    { state with stack = stack }

let mutateMemory state =
    let mem = Array.copy state.Memory
    { state with Memory = mem }

let redraw (state, (stateMutator: StateMutator)) =
    stateMutator.frameTypeMutator <- (fun _ -> FrameType.Drawable)
    state, stateMutator

let incrementPc (state, (stateMutator: StateMutator)) =
    stateMutator.pcMutator <- (fun t -> t + 2us)
    state, stateMutator


let hSetIndex idx (state, (stateMutator: StateMutator)) =
    stateMutator.IMutator <- (fun _ -> idx)
    state, stateMutator

let hJump value (state, (stateMutator: StateMutator)) =
    stateMutator.pcMutator <- (fun _ -> value)
    state, stateMutator

let hClearScreen (state, (stateMutator: StateMutator)) =
    stateMutator.gfxMutator <- (fun _ -> (Array.create 2048 0uy))
    state, stateMutator

let hAddToIndex idx (state, (stateMutator: StateMutator)) =
    stateMutator.IMutator <- (fun i -> i + uint16(state.V.[idx]))
    state, stateMutator

let hSetTimer x (state, (stateMutator: StateMutator)) =
    stateMutator.delayTimerMutator <- (fun _ -> state.V.[x])
    state, stateMutator

let hJumpRelative N (state, (stateMutator: StateMutator)) = 
    stateMutator.pcMutator <- (fun _ -> uint16(state.V.[0]) + N)
    state, stateMutator

let hSetSound x (state, (stateMutator: StateMutator)) =
    stateMutator.soundTimerMutator <- (fun _ -> state.V.[x])
    state, stateMutator

let hMoveToSprite x (state, (stateMutator: StateMutator)) = 
    stateMutator.IMutator <- (fun _ -> (uint16(state.V.[x]) * 5us) + 80us)
    state, stateMutator

let hJumpToSubroutine value  (state, (stateMutator: StateMutator)) = 
    let mutateStack = mutateArray  (fun s -> s.[int(state.sp)] <- state.pc)
    stateMutator.stackMutator <- mutateStack
    stateMutator.spMutator <- (fun s -> s + 1us)
    stateMutator.pcMutator <- (fun _ -> value)
    state, stateMutator

let handleAdd x y (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let yvalue = state.V.[y]
    let carry = if (uint16(xvalue) + uint16(yvalue)) > 0xFFus then 1uy else 0uy
    state.V.[x] <- (xvalue + yvalue)
    state.V.[0xF] <-carry
    state, stateMutator

let handleBinaryCode x  (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let moved = xvalue >>> 8
    state.Memory.[int32(state.I)] <- (moved / 100uy)
    state.Memory.[int32(state.I + 1us)] <- (moved / 10uy % 10uy)
    state.Memory.[int32(state.I + 2us)] <- (moved % 100uy % 10uy)
    state, stateMutator

let hReturnFromSubroutine  (state, (stateMutator: StateMutator)) = 
    let pointer = state.sp - 1us
    { state with sp = pointer ; pc = state.stack.[int32(pointer)] }
    state, stateMutator


let hSkipIfTrue addr value  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[addr] = value 
                then state.pc + 4us 
                else state.pc + 2us
    { state with pc = pc }
    state, stateMutator

let hSkipIfFalse addr value  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[addr] <> value 
                then state.pc + 4us 
                else state.pc + 2us
    { state with pc = pc }
    state, stateMutator

let hSkipIfRegisterEquals x y  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[x] = state.V.[y]
                then state.pc + 4us
                else state.pc + 2us
    { state with pc = pc }
    state, stateMutator

let hSetRegister addr value  (state, (stateMutator: StateMutator)) = 
    state.V.[addr] <- value
    state

let hAddNoCarry addr value  (state, (stateMutator: StateMutator)) = 
    state.V.[addr] <- state.V.[addr] + value
    state, stateMutator

let hAssign x y  (state, (stateMutator: StateMutator)) = 
    state.V.[x] <- state.V.[y]
    state, stateMutator

let hBitAnd x y  (state, (stateMutator: StateMutator)) = 
    state.V.[x] <- state.V.[x] &&& state.V.[y]
    state, stateMutator

let hBitOr x y  (state, (stateMutator: StateMutator)) = 
    state.V.[x] <- state.V.[x] ||| state.V.[y]
    state, stateMutator

let hBitshiftLeft x  (state, (stateMutator: StateMutator)) = 
    state.V.[0xF] <- state.V.[x] >>> 7
    state.V.[x] <- state.V.[x] <<< 1
    state, stateMutator

let hBitshiftRight x  (state, (stateMutator: StateMutator)) = 
    state.V.[0xF] <- state.V.[x] &&& 1uy
    state.V.[x] <- state.V.[x] >>> 1
    state, stateMutator

let hBitXor x y  (state, (stateMutator: StateMutator)) = 
    state.V.[x] <- state.V.[x] ^^^ state.V.[y]
    state, stateMutator

let hGetTimer x  (state, (stateMutator: StateMutator)) = 
    state.V.[x] <- state.delayTimer
    state, stateMutator

let hSkipIfRegisterNotEq x y  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[x] <> state.V.[y]
                then state.pc + 4us
                else state.pc + 2us
    { state with pc = pc }
    state, stateMutator

let hSubtract x y (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let yvalue = state.V.[y]
    let borrow = if (int16(xvalue) - int16(yvalue)) < 0s then 0uy else 1uy
    state.V.[x] <- (xvalue - yvalue)
    state.V.[0xF] <- borrow
    state, stateMutator

let hKeyPress x (keys: uint8 [])  (state, (stateMutator: StateMutator)) = 
    let pc = if keys.[int(state.V.[x])] = 1uy then state.pc + 4us else state.pc + 2us
    { state with pc = pc } 
    state, stateMutator

let hKeyNotPressed x (keys: uint8 [])  (state, (stateMutator: StateMutator)) = 
    let pc = if keys.[int(state.V.[x])] = 0uy then state.pc + 4us else state.pc + 2us
    { state with pc = pc } 
    state, stateMutator

let hSubtractFromY x y  (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let yvalue = state.V.[y]
    let borrow = if (int16(yvalue) - int16(xvalue)) < 0s then 0uy else 1uy
    state.V.[x] <- (yvalue - xvalue)
    state.V.[0xF] <- borrow
    state, stateMutator

let hKeyPressBlocking x keys  (state, (stateMutator: StateMutator)) = 
    let pressedKeyIdx = keys |> Seq.tryFindIndex (fun k -> k = 1uy)
    match pressedKeyIdx with
    | None      -> state
    | Some idx  -> state.V.[x] <- uint8(idx)
                   { state with pc = state.pc + 2us }
    state, stateMutator

let hDrawSprite startX startY height  (state, (stateMutator: StateMutator)) = 
    let x = int(state.V.[startX])
    let y = int(state.V.[startY])
    state.V.[0xF] <- 0uy
    // Loop over each ROW
    for yline = 0 to (height - 1) do
        // Fetch pixel value from memory starting at location I
        let mutable pixel = state.Memory.[int(state.I) + yline]

        // Loop over 8 bits pr row
        for xline = 0 to 7 do
            // Check if currently evaluated pixel is set to 1
            // 0x80uy >>> xline scans through byte one bit at the time
            if pixel &&& (0x80uy >>> xline) <> 0uy
            then
                // Get index, offset y value by width of screen (64px)
                let index = (x + xline + ((y + yline) * 64)) % 2048
                // If displayed pixel is on, register collision
                if state.gfx.[index] = 1uy 
                then 
                    state.V.[0xF] <- 1uy
                // Set pixel value using xor
                let newVal = state.gfx.[index] ^^^ 1uy
                state.gfx.[index] <- uint8(newVal)
    state, stateMutator

let hRand x n  (state, (stateMutator: StateMutator)) = 
    let rand = uint8(System.Random().Next(0,255))
    state.V.[x] <- rand &&& n
    state, stateMutator

let hRegDump x  (state, (stateMutator: StateMutator)) = 
    let startI = int(state.I)
    for i = 0 to x do
        state.Memory.[startI + i] <- state.V.[i]
    state, stateMutator

let hRegLoad x  (state, (stateMutator: StateMutator)) = 
    let startI = int(state.I)
    for i = 0 to x do
        state.V.[i] <- state.Memory.[startI + i]
    state, stateMutator