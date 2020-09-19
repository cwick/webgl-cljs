#version 300 es
// ^^ This must appear in the first line of a shader program written in GLSL ES
// version 3.00. If omitted, the shader will be treated as targeting version 1.00

in vec3 POSITION;
in vec2 TEXCOORD_0;
out vec3 v_worldPosition;
out vec2 v_texcoord_0;
uniform mat4 u_transform;
uniform mat4 u_projection;
uniform mat4 u_view;

void main() {
  vec4 world = u_transform * vec4(POSITION,1);
  gl_Position = u_projection * u_view * world;
  gl_PointSize = 20.;
  v_texcoord_0 = TEXCOORD_0;
  v_worldPosition = vec3(world);
}