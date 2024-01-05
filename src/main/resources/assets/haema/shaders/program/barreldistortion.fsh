#version 150

uniform sampler2D DiffuseSampler;

in vec2 texCoord;

// positive values give barrel distortion, negative give pincushion
uniform float DistortAmount;

out vec4 fragColor;

void main() {
    //Transform so 0,0 is center and edges are 1 away
    vec2 coord = (2.0 * texCoord) - 1.0;

    //Apply distortion
    float sqDist = (coord.x*coord.x) + (coord.y * coord.y);
    coord = (coord * ( 1.0 + (sqDist * DistortAmount)));

    //Transform so 0,0 is bottom left again
    coord = (1.0 + (coord))/2.0;

    fragColor = texture(DiffuseSampler, coord);
}
