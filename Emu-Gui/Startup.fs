namespace EmuGui

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open ElectronNET.API
open ElectronNET.API.Entities


type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member this.ElectronBootstrap =
        async {
            let! browserWindow = Electron.WindowManager.CreateWindowAsync (new BrowserWindowOptions(Show = false)) |> Async.AwaitTask
            browserWindow.add_OnReadyToShow( fun _ -> browserWindow.Show());
            browserWindow.SetTitle("Chip 8 Emulator")
        }

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddMvc() |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        app.UseStaticFiles() |> ignore
        app.UseMvc(fun routes -> 
            routes.MapRoute(name = "default", template= "{controller=Home}/{action=Index}/{id?}") |> ignore
        ) |> ignore
        if HybridSupport.IsElectronActive
        then Async.Start(this.ElectronBootstrap)

    member val Configuration : IConfiguration = null with get, set