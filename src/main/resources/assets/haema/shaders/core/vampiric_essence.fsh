#version 150

#moj_import <fog.glsl>

uniform sampler2D Sampler0;

uniform vec4 ColorModulator;
uniform float FogStart;
uniform float FogEnd;
uniform vec4 FogColor;

in float vertexDistance;
in vec4 vertexColor;
in vec2 texCoord0;
in vec4 normal;
in vec3 vertexPosition;

out vec4 fragColor;

float random (in vec3 _st) {
    return fract(sin(dot(_st.xyz,
    vec3(12.9898,78.233,15.34247)))*
    43758.5453123);
}

float random (in vec2 _st) {
    return fract(sin(dot(_st.xy,
    vec2(12.9898, 78.233)))*
    43758.5453123);
}

float random(in ivec2 _st) {
    return random(vec2(_st.x, _st.y));
}

// exponential smooth min (k=32)
float smin( float a, float b, float k )
{
    float res = exp2( -k*a ) + exp2( -k*b );
    return -log2( res )/k;
}


float voronoi( in vec3 x )
{
    ivec3 p = ivec3(floor( x ));
    vec3  f = fract( x );

    float res = 8.0;
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    for( int k=-1; k<=1; k++ )
    {
        ivec3 b = ivec3( i, j, k );
        vec3  r = vec3( b ) - f + random( p + b );
        float d = dot( r, r );

        res = smin( res, d, 32. );
    }
    return sqrt( res );
}

void main() {
    vec4 color = voronoi(vertexPosition) * vertexColor * ColorModulator;
    fragColor = linear_fog(color, vertexDistance, FogStart, FogEnd, FogColor);
}