﻿function setPixel(imageData, x, y, r, g, b, a) {
    index = (x + y * imageData.width) * 4;
    imageData.data[index + 0] = r;
    imageData.data[index + 1] = g;
    imageData.data[index + 2] = b;
    imageData.data[index + 3] = a;
}

function setPixelLinear(imageData, pos, val) {
    var index = pos * 4;
    //const pixelVal = val === 0 ? 0 : 255;
    imageData.data[index] = val;
    imageData.data[index + 1] = val;
    imageData.data[index + 2] = val;
    imageData.data[index + 3] = 255;
}

const screen = document.getElementById("screen");
const ctx = screen.getContext("2d");
const imageData = ctx.createImageData(64, 32);

for (var x = 0; x < 64; x++)
    for (var y = 0; y < 32; y++)
        setPixel(imageData, x, y, 0, 0, 0, 255);

ctx.putImageData(imageData, 0, 0);
//const smallScreenCtx = document.createElement("canvas").getContext("2d");
//smallScreenCtx.createImageData(64, 32);
//screen.style.width = '640px';
//screen.style.height = '320px';
function updateScreen(gfx) {
    for (var i = 0; i < gfx.length; i++) {
        setPixelLinear(imageData, i, gfx[i]);
    }
    ctx.putImageData(imageData, 0, 0);

}