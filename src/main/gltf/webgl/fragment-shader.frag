#version 300 es
 
// Fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

out vec4 outColor;
in vec3 worldPosition;
in vec2 texcoord_0;
uniform sampler2D u_texture0;
uniform vec4 u_baseColor;
uniform vec3 u_eyePosition;

vec4 gammaCorrect(vec4 c) {
  float gamma = 2.2;
  return vec4(
    pow(
      vec3(c), 
      vec3(1.0/gamma)), 
    c.a);
} 

vec3 calcNormal() {
  vec3 xTangent = dFdx(worldPosition);
  vec3 yTangent = dFdy(worldPosition);
  return normalize( cross( xTangent, yTangent ) );
}

void main() {
  vec3 normal = calcNormal();
  vec3 lightPosition = vec3(30,10,-10);
  vec3 lightVector = normalize(lightPosition - worldPosition);
  float lightMultiplier = clamp(dot(lightVector, normal), 0.0, 1.0);
  vec3 surfaceColor = 
    vec3(texture(u_texture0, texcoord_0)) * 
    vec3(u_baseColor);
  vec3 unlit = surfaceColor / 50.0;

  // TODO: deal with alpha channel
  outColor = vec4(
    unlit +
    lightMultiplier * surfaceColor
    , 1.0);

  // TODO: only gamma correct if we have to
  outColor = gammaCorrect(outColor);
}