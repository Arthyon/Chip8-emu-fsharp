namespace EmuGui.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open ElectronNET.API
open ElectronNET.API.Entities
open Program
open ElectronNET.API
// Game loop:
// in JS: SetInterval, fetch pressed keys and send via ipc
//  OR
// Set timer in F# and emit ipc-event from JS when key is pressed, and to update gfx <- seems better

// F#: Calculate next frame, persist state, send new gfx to js if necessary
// On blur: stop interval (via ipc from F#?)

type Home () =
    inherit Controller()

    member this.Index() =
        if HybridSupport.IsElectronActive then this.Initialize ()


        this.View()

    member this.Initialize() =
        this.InitializeMenu () |> ignore
        ()

    member this.InitializeMenu() =
        let menu = [|
            new MenuItem(
                Label = "File", 
                Submenu = [|
                    new MenuItem (Label = "Open ROM", Accelerator = "CmdOrCtrl+O", Click = fun _ -> Async.Start(this.OpenFile ()))
                    new MenuItem (Label = "Exit", Accelerator = "CmdOrCtrl+Q", Click = fun _ -> Electron.App.Quit())
                |]
            )
            new MenuItem(
                Label = "Dev",
                Submenu = [|
                    new MenuItem (Label = "Dev console", Accelerator = "F12", Click = fun _ -> Electron.WindowManager.BrowserWindows.First().WebContents.OpenDevTools())
                |]
            )
        |]
        
        Electron.Menu.SetApplicationMenu(menu)
        ()

    member this.OpenFile () =
        let mainWindow = Electron.WindowManager.BrowserWindows |> Seq.head
        let options = 
            new OpenDialogOptions(
                Properties = [|OpenDialogProperty.openFile|],
                Filters = [|new FileFilter(Name = "Chip8-ROMs", Extensions = [|"ch8"|])|]
            )
        async {
            let! file = Electron.Dialog.ShowOpenDialogAsync (mainWindow, options) |> Async.AwaitTask
            EmuGui.EmuWrapper.Instance.Load file.[0]
        }
