/*
CC-BY-NC-SA
https://www.shadertoy.com/view/MlSXR3#
*/

uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

uniform vec2 InSize;

// positive values give barrel distortion, negative give pincushion
uniform float DistortAmount;

void main() {
    vec2 uv = texCoord;
    //uv.y = 1.0 - uv.y;
    uv = uv * 2.0 - 1.0;

    float r2 = uv.x*uv.x + uv.y*uv.y;
    uv *= 1.0 + DistortAmount * r2;

    uv = 0.5 * (uv * 0.5 + 1.0);

    gl_FragColor = texture2D(DiffuseSampler, uv);
}