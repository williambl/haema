#version 150

uniform sampler2D DiffuseSampler;

uniform vec4 ColorModulate;

varying vec2 texCoord;

float random (in vec2 _st) {
    return fract(sin(dot(_st.xy,
    vec2(12.9898,78.233)))*
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


float voronoi( in vec2 x )
{
    ivec2 p = ivec2(floor( x ));
    vec2  f = fract( x );

    float res = 8.0;
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        ivec2 b = ivec2( i, j );
        vec2  r = vec2( b ) - f + random( p + b );
        float d = dot( r, r );

        res = smin( res, d, 32. );
    }
    return sqrt( res );
}

void main() {
    float d = voronoi(texCoord * 10.0);

    gl_FragColor = vec4(vec3(d), texture2D(DiffuseSampler, texCoord).a*d) * ColorModulate;
}