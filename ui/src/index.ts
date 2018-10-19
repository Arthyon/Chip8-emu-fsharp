import * as electron from "electron";
import * as path from "path";
let win: Electron.BrowserWindow | null;

function createWindow() {
  win = new electron.BrowserWindow({ width: 800, height: 600 });
  win.loadFile(path.join(__dirname, "../views/index.html"));
  win.webContents.openDevTools();

  // Emitted when the window is closed.
  win.on("closed", () => {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    win = null;
  });
}

electron.app.on("ready", createWindow);
electron.app.on("window-all-closed", () => {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== "darwin") {
    electron.app.quit();
  }
});

electron.app.on("activate", () => {
  // On OS X it"s common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (win === null) {
    createWindow();
  }
});
