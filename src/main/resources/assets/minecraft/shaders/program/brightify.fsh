#version 120

uniform sampler2D DiffuseSampler;

varying vec2 texCoord;
varying vec2 oneTexel;

uniform vec2 InSize;

uniform vec3 RedBrightnessMap   = vec3(5.0, 2.0, 1.0);
uniform vec3 GreenBrightnessMap = vec3(5.0, 2.0, 1.0);
uniform vec3 BlueBrightnessMap  = vec3(5.0, 2.0, 1.0);

float applyBrightnessMap(float valueIn, float brightness, vec3 brightnessMap) {
    if (brightness < 0.33) {
        return valueIn * brightnessMap[0];
    }
    if (brightness < 0.66) {
        return valueIn * brightnessMap[1];
    }
    return valueIn * brightnessMap[2];
}

void main() {
    vec4 InTexel = texture2D(DiffuseSampler, texCoord);

    // Color Matrix
    float overallBrightness = (0.299*InTexel.r + 0.587*InTexel.g + 0.114*InTexel.b);
    float RedValue = applyBrightnessMap(InTexel.r, overallBrightness, RedBrightnessMap);
    float GreenValue = applyBrightnessMap(InTexel.g, overallBrightness, GreenBrightnessMap);
    float BlueValue = applyBrightnessMap(InTexel.b, overallBrightness, BlueBrightnessMap);
    vec3 OutColor = vec3(RedValue, GreenValue, BlueValue);

    gl_FragColor = vec4(OutColor, 1.0);
}

