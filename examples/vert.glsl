#version 120

void main() {
	gl_Position = gl_ModelViewMatrix * gl_Vertex;
}
