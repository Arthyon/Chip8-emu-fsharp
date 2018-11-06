const { ipcRenderer } = require("electron");
var now,
    dt = 0,
    last = window.performance.now,
    step = 1 / 60,
    slow = 1; // slow motion scaling factor (5 = 5 times slower)

var isPaused = true;
var rewind = false;


document.addEventListener('keydown', (event) => {
    const res = handleInput(event.key, 1);
    if (res.pauseButton) {
        if (isPaused)
            start()
        else
            isPaused = true;
    } else if (res.rewindButton) {
        rewind = true;
    } else if (res.stepForwardButton) {
        isPaused = true;

        ipcRenderer.send('tick', { keys, rewind });
    }
});

document.addEventListener('keyup', (event) => {
    const res = handleInput(event.key, 0);
    if (res.rewindButton) {
        rewind = false;
    }
});

function run(timestamp) {
    if (isPaused) return;

    now = window.performance.now();
    dt = dt + Math.min(1, (now - last) / 1000);
    let slowStep = (step * slow);
    while (dt > slowStep) {
        if (isPaused) break;
        dt = dt - slowStep;
        ipcRenderer.send('tick', { keys, rewind });
    }
    last = now
    window.requestAnimationFrame(run);
}
ipcRenderer.on('failure', (event, arg) => {
    isPaused = true;
    document.getElementById('fail').innerHTML = arg;

});
ipcRenderer.on('status', (event, arg) => {
    document.getElementById('status').innerHTML = arg;
});
ipcRenderer.on('console', (event, arg) => {
    console.log(arg);

});

ipcRenderer.on('beep', (event, arg) => {
    document.getElementById('beep').innerHTML = "BEEP";
    setTimeout(() => document.getElementById('beep').innerHTML = "", 500);
});

// ipcRenderer.on('endBeep', (event, arg) => {
//     document.getElementById('beep').innerHTML = "";
// });

ipcRenderer.on('start', (event, arg) => {
    isPaused = true;
    // start();
});

ipcRenderer.on('update-gfx', (event, arg) => {
    updateScreen(arg);
});

function start() {
    isPaused = false;
    last = window.performance.now();
    window.requestAnimationFrame(run);
}
