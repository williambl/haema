uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

uniform vec2 InSize;
uniform vec2 OutSize;

// positive values give barrel distortion, negative give pincushion
uniform float DistortAmount;

void main() {
    gl_FragColor = texture2D(DiffuseSampler, texCoord);
}