#version 120

uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

uniform vec2 InSize;

uniform float Gamma = 1.0;

void main() {
    vec4 InTexel = texture2D(DiffuseSampler, texCoord);

    gl_FragColor = vec4(pow(InTexel.r, Gamma),pow(InTexel.g, Gamma),pow(InTexel.b, Gamma), 1.0);
}

