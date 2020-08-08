#version 300 es
// ^^ This must appear in the first line of a shader program written in GLSL ES
// version 3.00. If omitted, the shader will be treated as targeting version 1.00

in vec3 POSITION;
out vec4 worldPosition;
uniform mat4 u_transform;
uniform mat4 u_projection;
uniform mat4 u_view;

void main() {
  worldPosition = u_transform * vec4(POSITION,1);
  gl_Position = u_projection * u_view * worldPosition;
}