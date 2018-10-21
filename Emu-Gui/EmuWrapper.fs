namespace EmuGui
    open Program
    open System.IO
    open ElectronNET.API
    open System

    type EmuWrapper private () =
        let currentKeys = Initialization.Initialization.initialInput

        let isPaused = false
        let mainWindow = Electron.WindowManager.BrowserWindows |> Seq.head

        let updateCurrentKeys arg =
            ()
        
        let getCurrentInput previousInput =
            Chip8.KeyInput.Pause
            // if isPaused 
            // then Chip8.KeyInput.Pause
            // else currentKeys

        let updateStatus (statusMsg: string) =
            Electron.IpcMain.Send(mainWindow, "status", statusMsg)

        let handleState (state: Chip8.State) =
            Electron.IpcMain.Send(mainWindow, "update-gfx", state.gfx |> Seq.map (fun p -> if p then 255 else 0))
        
        static let instance = EmuWrapper ()
        static member Instance = instance

        member this.Load path = 
            updateStatus "Loading file..."
            let bytes = File.ReadAllBytes path
            let validationResult = ValidateRom bytes 0
            match validationResult with
            | Some s    -> Electron.Dialog.ShowErrorBox("Error loading rom", s)
            | None      -> this.InitializeSession bytes

        member this.InitializeSession bytes =
            updateStatus "Initializing"
            Electron.IpcMain.On("keypress", fun args -> updateCurrentKeys args)
            Run bytes getCurrentInput handleState |> ignore
