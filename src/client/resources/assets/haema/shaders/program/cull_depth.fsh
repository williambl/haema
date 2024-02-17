#version 150

uniform sampler2D DiffuseSampler;
uniform sampler2D DiffuseDepthSampler;
uniform sampler2D MainDepthSampler;

in vec2 texCoord;
in vec2 oneTexel;

uniform vec2 InSize;

out vec4 fragColor;

void main() {
    vec4 CurrTexel = texture(DiffuseSampler, texCoord);
    float CurrDepth = texture(DiffuseDepthSampler, texCoord).r;
    float BgDepth = texture(MainDepthSampler, texCoord).r;

    fragColor = CurrDepth < BgDepth ? CurrTexel : vec4(0.0);
}
