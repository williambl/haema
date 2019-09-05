#version 120

uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

void main(){
    vec4 center = texture2D(DiffuseSampler, texCoord);
    vec4 left   = texture2D(DiffuseSampler, texCoord - vec2(oneTexel.x, 0.0));
    vec4 right  = texture2D(DiffuseSampler, texCoord + vec2(oneTexel.x, 0.0));
    vec4 up     = texture2D(DiffuseSampler, texCoord - vec2(0.0, oneTexel.y));
    vec4 down   = texture2D(DiffuseSampler, texCoord + vec2(0.0, oneTexel.y));
    vec4 leftDiff  = center - left;
    vec4 rightDiff = center - right;
    vec4 upDiff    = center - up;
    vec4 downDiff  = center - down;
    vec4 totalDiff = clamp(leftDiff + rightDiff + upDiff + downDiff, 0.0, 1.0);

    vec4 gray = vec4(0.5, 0.5, 0.5, 0.0);
    float totalDiffLuma = 0.5+dot(totalDiff, gray);

    vec4 final = mix(center, totalDiff, totalDiffLuma);

    gl_FragColor = vec4(final.rgb, 1.0);
}
