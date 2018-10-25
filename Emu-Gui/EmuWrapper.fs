namespace EmuGui
    open Program
    open System.IO
    open ElectronNET.API
    open Chip8
    open Newtonsoft.Json.Linq

    type EmuWrapper private () =
        let mainWindow = Electron.WindowManager.BrowserWindows |> Seq.head

        let mutable currentState = None
        let mutable previousStates = []

        let updateStatus (statusMsg: string) =
            Electron.IpcMain.Send(mainWindow, "status", statusMsg)
        let console (object: obj)=
            Electron.IpcMain.Send(mainWindow, "console", (sprintf "%A" object))

        let draw (state: Chip8.State) =
            Electron.IpcMain.Send(mainWindow, "update-gfx", state.gfx |> Seq.map (fun p -> if p then 255 else 0))
        
        let TryPlaySound state =
            if state.soundTimer = 1uy 
            then Electron.IpcMain.Send(mainWindow, "beep", "")
        
        let fail (msg: string) =
            Electron.IpcMain.Send(mainWindow, "failure", msg)
            

        let UpdateState currentKeys =
            match currentState with
            | None      ->  updateStatus "Emu not initialized"
            | Some s    ->  let prevStates, newState = StepGameLoop previousStates currentKeys s
                            if fst newState.terminating 
                            then
                                ()
                                //fail (sprintf "Failure: %s" <| snd newState.terminating)
                                //currentState <- None
                            else
                                currentState <- Some newState
                                previousStates <- prevStates
                                TryPlaySound newState
                                match newState.frameType with
                                | FrameType.Drawable        -> draw newState
                                | FrameType.Computational   -> ()

        let keyArrayToKeyInput (object : obj) =
            let arr =   match object with
                        | :? JArray as a -> a |> Seq.map (fun v -> uint8(v))|> Seq.toArray
                        | _              -> [||]
            KeyInput.NormalPlay arr

        static let instance = EmuWrapper ()
        static member Instance = instance

        member this.Load path = 
            updateStatus "Loading file..."
            let bytes = File.ReadAllBytes path
            let validationResult = ValidateRom bytes 0
            match validationResult with
            | Some s    -> fail (sprintf "Error loading rom: %s" s)
            | None      -> this.InitializeSession bytes

        member this.InitializeSession bytes =
            updateStatus "Initializing"
            Electron.IpcMain.RemoveAllListeners("tick");
            Electron.IpcMain.On("tick", fun keys -> UpdateState (keyArrayToKeyInput keys))
            let prevStates, state = InitEmu bytes 
            currentState <- Some state
            previousStates <- prevStates
            updateStatus "Running"
            Electron.IpcMain.Send(mainWindow, "start", "")
