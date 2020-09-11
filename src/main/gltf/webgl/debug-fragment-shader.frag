#version 300 es
 
// Fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

out vec4 outColor;

vec4 gammaCorrect(vec4 c) {
  float gamma = 2.2;
  return vec4(
    pow(
      vec3(c), 
      vec3(1.0/gamma)), 
    c.a);
} 

void main() {
  outColor = vec4(1.,1.,1.,1.);
  outColor = gammaCorrect(outColor);
}