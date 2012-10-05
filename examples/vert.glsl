#version 330

void main() {
	gl_Position   = gl_ModelViewMatrix * gl_Vertex;
	gl_FrontColor = gl_Color;
}
