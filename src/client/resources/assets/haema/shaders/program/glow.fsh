#version 150

uniform sampler2D DiffuseSampler;

in vec2 texCoord;
in vec2 oneTexel;

uniform vec2 InSize;
uniform float Time;
uniform float Saturation;

out vec4 fragColor;

void main() {
    vec4 CurrTexel = texture(DiffuseSampler, texCoord) * vec4(Saturation, 1./Saturation, 1./Saturation, 1.0);

    fragColor = vec4(CurrTexel.rgb, length(CurrTexel.rgb)/(3*3));
}