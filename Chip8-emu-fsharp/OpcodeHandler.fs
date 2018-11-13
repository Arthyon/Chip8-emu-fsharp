module OpcodeHandler

let mutateArray mutator arr =
    let newArray = Array.copy arr
    mutator newArray
    newArray
    
    
let redraw (state, (stateMutator: StateMutator)) =
    stateMutator.frameTypeMutator <- (fun _ -> FrameType.Drawable)
    state, stateMutator

let incrementPc (state, (stateMutator: StateMutator)) =
    stateMutator.pcMutator <- stateMutator.pcMutator >> (fun t -> t + 2us)
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
    let mutateRegister = mutateArray (fun v ->
        v.[x] <- (xvalue + yvalue)
        v.[0xF] <- carry
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let handleBinaryCode x  (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let moved = xvalue >>> 8
    let mutateMemory = mutateArray (fun m ->
        m.[int(state.I)] <- (moved / 100uy)
        m.[int(state.I + 1us)] <- (moved / 10uy % 10uy)
        m.[int(state.I + 2us)] <- (moved % 100uy % 10uy)
    )
    stateMutator.MemoryMutator <- mutateMemory
    state, stateMutator

let hReturnFromSubroutine  (state, (stateMutator: StateMutator)) = 
    let pointer = state.sp - 1us
    stateMutator.spMutator <- (fun s -> s - 1us)
    stateMutator.pcMutator <- (fun _ -> state.stack.[int(pointer)])
    state, stateMutator


let hSkipIfTrue addr value  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[addr] = value 
                then state.pc + 4us 
                else state.pc + 2us
    stateMutator.pcMutator <- (fun _ -> pc)
    state, stateMutator

let hSkipIfFalse addr value  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[addr] <> value 
                then state.pc + 4us 
                else state.pc + 2us
    stateMutator.pcMutator <- (fun _ -> pc)
    state, stateMutator

let hSkipIfRegisterEquals x y  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[x] = state.V.[y]
                then state.pc + 4us
                else state.pc + 2us
    stateMutator.pcMutator <- (fun _ -> pc)
    state, stateMutator

let hSetRegister addr value  (state, (stateMutator: StateMutator)) = 
    let registerMutator = mutateArray (fun v -> v.[addr] <- value)
    stateMutator.VMutator <- registerMutator
    state, stateMutator

let hAddNoCarry addr value  (state, (stateMutator: StateMutator)) = 
    let registerMutator = mutateArray (fun v -> v.[addr] <- v.[addr] + value)
    stateMutator.VMutator <- registerMutator
    state, stateMutator

let hAssign x y  (state, (stateMutator: StateMutator)) = 
    let registerMutator = mutateArray (fun v -> v.[x] <- v.[y])
    stateMutator.VMutator <- registerMutator
    state, stateMutator

let hBitAnd x y  (state, (stateMutator: StateMutator)) = 
    let registerMutator = mutateArray (fun v -> v.[x] <- v.[x] &&& v.[y])
    stateMutator.VMutator <- registerMutator
    state, stateMutator

let hBitOr x y  (state, (stateMutator: StateMutator)) = 
    let registerMutator = mutateArray (fun v -> v.[x] <- v.[x] ||| v.[y])
    stateMutator.VMutator <- registerMutator
    state, stateMutator

let hBitshiftLeft x  (state, (stateMutator: StateMutator)) = 
    let mutateRegister = mutateArray (fun v ->
        v.[0xF] <- v.[x] >>> 7
        v.[x] <- v.[x] <<< 1
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hBitshiftRight x  (state, (stateMutator: StateMutator)) = 
    let mutateRegister = mutateArray (fun v ->
        v.[0xF] <- v.[x] &&& 1uy
        v.[x] <- v.[x] >>> 1
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hBitXor x y  (state, (stateMutator: StateMutator)) = 
    let mutateRegister = mutateArray (fun v ->
        v.[x] <- v.[x] ^^^ v.[y]
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hGetTimer x  (state, (stateMutator: StateMutator)) = 
    let mutateRegister = mutateArray (fun v ->
        v.[x] <- state.delayTimer
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hSkipIfRegisterNotEq x y  (state, (stateMutator: StateMutator)) = 
    let pc =    if state.V.[x] <> state.V.[y]
                then state.pc + 4us
                else state.pc + 2us
    stateMutator.pcMutator <- (fun _ -> pc)
    state, stateMutator

let hSubtract x y (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let yvalue = state.V.[y]
    let borrow = if (int16(xvalue) - int16(yvalue)) < 0s then 0uy else 1uy
    let mutateRegister = mutateArray (fun v ->
        v.[x] <- (xvalue - yvalue)
        v.[0xF] <- borrow
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hKeyPress x (keys: uint8 [])  (state, (stateMutator: StateMutator)) = 
    let pc = if keys.[int(state.V.[x])] = 1uy then state.pc + 4us else state.pc + 2us
    stateMutator.pcMutator <- (fun _ -> pc)
    state, stateMutator

let hKeyNotPressed x (keys: uint8 [])  (state, (stateMutator: StateMutator)) = 
    let pc = if keys.[int(state.V.[x])] = 0uy then state.pc + 4us else state.pc + 2us
    stateMutator.pcMutator <- (fun _ -> pc)
    state, stateMutator

let hSubtractFromY x y  (state, (stateMutator: StateMutator)) = 
    let xvalue = state.V.[x]
    let yvalue = state.V.[y]
    let borrow = if (int16(yvalue) - int16(xvalue)) < 0s then 0uy else 1uy
    let mutateRegister = mutateArray (fun v ->
        v.[x] <- (yvalue - xvalue)
        v.[0xF] <- borrow
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hKeyPressBlocking x keys  (state, (stateMutator: StateMutator)) = 
    let pressedKeyIdx = keys |> Seq.tryFindIndex (fun k -> k = 1uy)
    match pressedKeyIdx with
    | None      ->  ()
    | Some idx  ->  state.V.[x] <- uint8(idx)

                    let mutateRegister = mutateArray (fun v ->
                        v.[x] <- uint8(idx)
                    )
                    stateMutator.VMutator <- mutateRegister
                    stateMutator.pcMutator <- (fun pc -> pc + 2us)
    state, stateMutator

let hDrawSprite startX startY height  (state, (stateMutator: StateMutator)) = 
    let x = int(state.V.[startX])
    let y = int(state.V.[startY])
    let mutable collision = 0uy
    let gfx = Array.copy state.gfx
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
                if gfx.[index] = 1uy 
                then 
                    collision <- 1uy
                // Set pixel value using xor
                let newVal = state.gfx.[index] ^^^ 1uy
                gfx.[index] <- uint8(newVal)
    let mutateRegister = mutateArray (fun v -> v.[0xF] <- collision)
    stateMutator.VMutator <- mutateRegister
    stateMutator.gfxMutator <- (fun _ -> gfx)
    state, stateMutator

let hRand x n  (state, (stateMutator: StateMutator)) = 
    let rand = uint8(System.Random().Next(0,255))
    let mutateRegister = mutateArray (fun v ->
        v.[x] <- rand &&& n
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator

let hRegDump x  (state, (stateMutator: StateMutator)) = 
    let startI = int(state.I)
    let mutateMemory = mutateArray (fun mem ->
        for i = 0 to x do
            mem.[startI + i] <- state.V.[i]
    )
    stateMutator.MemoryMutator <- mutateMemory
    state, stateMutator

let hRegLoad x  (state, (stateMutator: StateMutator)) = 
    let startI = int(state.I)
    let mutateRegister = mutateArray (fun v ->
        for i = 0 to x do
            v.[i] <- state.Memory.[startI + i]
    )
    stateMutator.VMutator <- mutateRegister
    state, stateMutator