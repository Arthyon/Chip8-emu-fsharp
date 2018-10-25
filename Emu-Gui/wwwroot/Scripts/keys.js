
let keys = new Array(16).fill(0);


function handleInput(key, value) {
    let idx = -1;
    switch (key) {
        case "1":
            idx = 0;
            break;
        case "2":
            idx = 1;
            break;
        case "3":
            idx = 2;
            break;
        case "4":
            idx = 3;
            break;
        case "q":
            idx = 4;
            break;
        case "w":
            idx = 5;
            break;
        case "e":
            idx = 6;
            break;
        case "r":
            idx = 7;
            break;
        case "a":
            idx = 8;
            break;
        case "s":
            idx = 9;
            break;
        case "d":
            idx = 10;
            break;
        case "f":
            idx = 11;
            break;
        case "z":
            idx = 12;
            break;
        case "x":
            idx = 13;
            break;
        case "c":
            idx = 14;
            break;
        case "v":
            idx = 15;
            break;
        case " ":
            return { pauseButton: true, rewindButton: false };
    }

    if (idx != -1)
        keys[idx] = value;

    return { pauseButton: false, rewindButton: false };

}