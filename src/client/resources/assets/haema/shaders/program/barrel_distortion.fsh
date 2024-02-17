#version 150

uniform sampler2D DiffuseSampler;

in vec2 texCoord;
in vec2 oneTexel;

uniform vec2 InSize;
uniform vec2 OutSize;

// positive values give barrel distortion, negative give pincushion
uniform float DistortAmount;

out vec4 fragColor;

void main() {
    //Transform so 0,0 is center and edges are 1 away
    vec2 coord = (2.0 * texCoord) - 1.0;

    //Apply distortion
    float sqDist = (coord.x*coord.x) + (coord.y * coord.y);

    int sampleCount = 3;
    vec4 accumulated = vec4(0.0);

    for (int i = 1; i < 1 + sampleCount; i++) {
        float distortion = DistortAmount * ((5+i)/6.);
        vec2 distoredCoord = (coord * (1.0 + (sqDist * distortion)));

        //Transform so 0,0 is bottom left again
        distoredCoord = (1.0 + (distoredCoord))/2.0;

        vec4 thisSample = texture(DiffuseSampler, distoredCoord);
        accumulated += thisSample*(i/sampleCount)*(1.0+abs(sqDist * distortion));
    }

    fragColor = accumulated;
}