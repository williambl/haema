/*
CC-BY-NC-SA
https://www.shadertoy.com/view/MlSXR3#
*/

uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

uniform vec2 InSize;
uniform vec2 OutSize;

// positive values give barrel distortion, negative give pincushion
uniform float DistortAmount;

void main() {
    /* @ 0:
    a   d
    |
    |
    b---c
    */
    vec2 coord = texCoord * 2.0 - vec2(1.0, 1.0);

    /*
    a   |   d
        |
        |
    ----+----
        |
        |
    b   |   c
    */
    float squareDistFromOrigin = coord.x*coord.x + coord.y*coord.y;
    coord *= 1.0 + (DistortAmount * squareDistFromOrigin);

    /* @ 0:
    a   |   d
        |
        |
    ----+----
        |
        |
    b   |   c
    */
    coord = 0.5 * (coord + vec2(1.0, 1.0));

    /* @ 0:
    a   d
    |
    |
    b---c
    */
    gl_FragColor = texture2D(DiffuseSampler, coord);
}