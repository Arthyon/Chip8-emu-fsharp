function setPixel(imageData, x, y, r, g, b, a) {
    index = (x + y * imageData.width) * 4;
    imageData.data[index + 0] = r;
    imageData.data[index + 1] = g;
    imageData.data[index + 2] = b;
    imageData.data[index + 3] = a;
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



}