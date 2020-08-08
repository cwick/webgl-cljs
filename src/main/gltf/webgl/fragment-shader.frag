#version 300 es
 
// Fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

out vec4 outColor;
in vec4 worldPosition;
   
void main() {
  vec3 xTangent = dFdx( vec3(worldPosition) );
  vec3 yTangent = dFdy( vec3(worldPosition) );
  vec3 faceNormal = normalize( cross( xTangent, yTangent ) );
  // outColor = vec4(gl_FragCoord.x / 800.0, gl_FragCoord.y / 600.0, 1, 1.0);
  float lightValue = smoothstep(-1.1, 1.2, faceNormal.y);
  outColor = vec4(vec2(lightValue), 1., 1.);

  // float v = smoothstep(0., 1.0, gl_FragCoord.z);
  // outColor = vec4(vec3(v), 1.);
}