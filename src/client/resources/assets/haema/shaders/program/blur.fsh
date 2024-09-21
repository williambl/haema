#version 150

uniform sampler2D DiffuseSampler;

in vec2 texCoord;
in vec2 oneTexel;

uniform vec2 InSize;

uniform vec2 BlurDir;
uniform float Radius;

out vec4 fragColor;

// this blur handles alpha better than vanilla's by not taking into account colour of transparent areas
void main() {
    vec4 blurred = vec4(0.0);
    float totalStrength = 0.0;
    float totalAlpha = 0.0;
    float totalSamples = 0.0;
    float maxExpectedAlpha = Radius * 2.0 + 1.0;
    for(float r = -Radius; r <= Radius; r += 1.0) {
        vec4 sampleValue = texture(DiffuseSampler, texCoord + oneTexel * r * BlurDir);


		// Accumulate smoothed blur
        float strength = 1.0 - abs(r / Radius);
        totalStrength = totalStrength + strength;
        blurred = blurred + sampleValue * strength;

        // Accumulate average alpha
        totalAlpha = totalAlpha + sampleValue.a * strength;
        totalSamples = totalSamples + 1.0;
    }
    fragColor = vec4(blurred.rgb / totalAlpha, totalAlpha);
}
