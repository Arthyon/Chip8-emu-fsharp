namespace EmuGui.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open ElectronNET.API
open ElectronNET.API.Entities
open Program
// Game loop:
// in JS: SetInterval, fetch pressed keys and send via ipc
//  OR
// Set timer in F# and emit ipc-event from JS when key is pressed, and to update gfx <- seems better

// F#: Calculate next frame, persist state, send new gfx to js if necessary
// On blur: stop interval (via ipc from F#?)

type Home () =
    inherit Controller()

    member this.Index() =
        if HybridSupport.IsElectronActive then this.InitializeMenu ()

        this.View()

    member this.InitializeMenu() =
        let menu = [|
            new MenuItem(
                Label = "File", 
                Submenu = [|
                    new MenuItem (Label = "Open", Accelerator = "CmdOrCtrl+O", Click = fun _ -> Async.Start(this.OpenFile ()))
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
            let status = main file.[0]
            ()
        }
