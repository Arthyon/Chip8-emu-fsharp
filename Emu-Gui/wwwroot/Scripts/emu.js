const { ipcRenderer } = require("electron");
var now,
    dt = 0,
    last = window.performance.now,
    step = 1 / 60,
    slow = 1; // slow motion scaling factor (5 = 5 times slower)

var isPaused = true;
var rewind = false;

var audio = new AudioContext();
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
    alert(arg);

});
ipcRenderer.on('status', (event, arg) => {
    console.log(arg);
});
ipcRenderer.on('console', (event, arg) => {
    console.log(arg);
});

ipcRenderer.on('beep', (event, arg) => {
    var o = audio.createOscillator();
    var g = audio.createGain();
    o.type = "square";
    o.connect(g);
    o.frequency.value = 440;
    g.connect(audio.destination);
    o.start(0);
    g.gain.exponentialRampToValueAtTime(0.00001, audio.currentTime + 0.08)
});

ipcRenderer.on('start', (event, arg) => {
     start();
});

ipcRenderer.on('update-gfx', (event, arg) => {
    updateScreen(arg);
});

function start() {
    isPaused = false;
    last = window.performance.now();
    window.requestAnimationFrame(run);
}
