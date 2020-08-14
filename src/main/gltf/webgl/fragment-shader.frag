#version 300 es
 
// Fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

out vec4 outColor;
in vec4 worldPosition;
in vec2 texcoord_0;
uniform sampler2D u_texture0;

vec4 gammaCorrect(vec4 c) {
  float gamma = 2.2;
  return vec4(
    pow(
      vec3(c), 
      vec3(1.0/gamma)), 
    c.a);
} 

void main() {
  vec3 xTangent = dFdx( vec3(worldPosition) );
  vec3 yTangent = dFdy( vec3(worldPosition) );
  vec3 faceNormal = normalize( cross( xTangent, yTangent ) );
  float lightValue = smoothstep(-1.0, 1.0, faceNormal.y);

  outColor = vec4(lightValue * vec3(texture(u_texture0, texcoord_0)), 1.0);

  // TODO: only gamma correct if we have to
  outColor = gammaCorrect(outColor);
}