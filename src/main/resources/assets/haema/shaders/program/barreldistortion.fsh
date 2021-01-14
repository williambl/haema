uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

uniform vec2 InSize;
uniform vec2 OutSize;

// positive values give barrel distortion, negative give pincushion
uniform float DistortAmount;

void main() {
    //Transform so 0,0 is center and edges are 1 away
    vec2 coord = (2.0 * texCoord) - 1.0;

    //Apply distortion
    float sqDist = (coord.x*coord.x) + (coord.y * coord.y);
    coord = (coord * ( 1.0 + (sqDist * DistortAmount)));

    //Transform so 0,0 is bottom left again
    coord = (1.0 + (coord))/2.0;

    gl_FragColor = texture2D(DiffuseSampler, coord);
}