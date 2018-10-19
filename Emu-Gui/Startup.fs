namespace Emu_Gui

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open ElectronNET.API


type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration
    member this.StartElectron =
        async {
            do! (Electron.WindowManager.CreateWindowAsync ()) |> Async.AwaitTask |> Async.Ignore
        }
    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddMvc() |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        app.UseMvc() |> ignore
        Async.Start(this.StartElectron)

    member val Configuration : IConfiguration = null with get, set