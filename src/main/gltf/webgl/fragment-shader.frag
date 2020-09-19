#version 300 es
 
// Fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

layout(location = 0) out vec4 outColor;
layout(location = 1) out vec3 o_depthValue;
in vec3 v_worldPosition;
in vec2 v_texcoord_0;
uniform sampler2D u_texture0;
uniform vec4 u_baseColor;
// TODO: start using this for specular
//uniform vec3 u_eyePosition;

// ----------------------------
// Taken from Stack Overflow
// https://stackoverflow.com/questions/47945303/how-to-access-the-values-of-the-depth-buffer-outside-of-webgl-context
// (I have no idea how this works)
vec3 packDepthToRGB(float depth)
{
    float depthVal = depth * (256.0*256.0*256.0 - 1.0) / (256.0*256.0*256.0);
    vec4 encode    = fract(depthVal * vec4(1.0, 256.0, 256.0*256.0, 256.0*256.0*256.0));
    return encode.xyz - encode.yzw / 256.0 + 1.0/512.0;
}
// ----------------------------

vec4 gammaCorrect(vec4 c) {
  float gamma = 2.2;
  return vec4(
    pow(
      vec3(c), 
      vec3(1.0/gamma)), 
    c.a);
} 

vec3 calcNormal() {
  vec3 xTangent = dFdx(v_worldPosition);
  vec3 yTangent = dFdy(v_worldPosition);
  return normalize( cross( xTangent, yTangent ) );
}

float calcLightIntensity() {
  vec3 normal = calcNormal();
  vec3 lightPosition = vec3(30,10,-10);
  vec3 lightVector = normalize(lightPosition - v_worldPosition);
  return clamp(dot(lightVector, normal), 0.0, 1.0);
}

void main() {
  vec3 surfaceColor = 
    vec3(texture(u_texture0, v_texcoord_0)) * 
    vec3(u_baseColor);
  vec3 unlit = surfaceColor / 50.0;

  outColor = vec4(
    unlit +
    calcLightIntensity() * surfaceColor
    , 1.0);

  // TODO: only gamma correct if we have to
  outColor = gammaCorrect(outColor);
  // Write depth value (0.0 - 1.0) for use in picking operations
  o_depthValue = packDepthToRGB(gl_FragCoord.z);
}