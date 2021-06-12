#version 330 core

#include "simple.ysl.gen.glsl"

struct Foo {
  vec3 foo;
  vec2 hh;
};

Foo foo;

void main() {
    foo.foo = vec3(0.0);
}