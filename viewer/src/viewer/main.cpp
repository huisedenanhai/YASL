#include "../common/application.hpp"
#include "../common/mesh.hpp"
#include "../common/shader.hpp"
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <filesystem>
#include <fstream>
#include <imgui/imgui.h>
#include <iostream>
#include <optional>
#include <shaderc/shaderc.hpp>
#include <spirv_glsl.hpp>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <vector>

static const char *vertex_shader_text = R"(
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 3) in vec2 uv;

out vec2 v_uv;

void main() {
  gl_Position = vec4(position, 1.0);
  v_uv = uv;
}
)";

static const char *fragment_shader_text = R"(
#version 330 core

in vec2 v_uv;

layout(location = 0) out vec4 color;

void main() {
  color = vec4(v_uv, 0.0, 1.0);
}
)";

namespace fs = std::filesystem;

std::string load_frag_shader(const fs::path &path) {
  std::ifstream f(path);
  std::string str((std::istreambuf_iterator<char>(f)),
                  std::istreambuf_iterator<char>());
  int version = 140;
  auto compile_to_spirv = [&](shaderc_optimization_level opt_level) {
    shaderc::Compiler compiler{};
    shaderc::CompileOptions options{};
    options.SetOptimizationLevel(opt_level);
    options.SetTargetEnvironment(shaderc_target_env_opengl, version);
    auto res =
        compiler.CompileGlslToSpv(str,
                                  shaderc_shader_kind::shaderc_fragment_shader,
                                  path.string().c_str(),
                                  options);
    if (res.GetCompilationStatus() != shaderc_compilation_status_success) {
      throw std::runtime_error(res.GetErrorMessage());
    }
    return std::vector<uint32_t>(res.begin(), res.end());
  };

  std::unordered_map<unsigned int, std::string> uniform_names{};
  {
    // compile without optimization to extract reflection info
    auto spirv = compile_to_spirv(shaderc_optimization_level_zero);
    spirv_cross::Compiler reflection(spirv.data(), spirv.size());
    for (auto var : reflection.get_active_interface_variables()) {
      if (reflection.has_decoration(var, spv::DecorationLocation) &&
          reflection.get_storage_class(var) ==
              spv::StorageClassUniformConstant) {
        auto location = reflection.get_decoration(var, spv::DecorationLocation);
        uniform_names[location] = reflection.get_name(var);
      }
    }
  }
  auto spirv = compile_to_spirv(shaderc_optimization_level_performance);
  spirv_cross::CompilerGLSL glsl_compiler(spirv.data(), spirv.size());
  std::unordered_map<unsigned int, std::string> input_names = {{0, "v_uv"}};
  for (auto var : glsl_compiler.get_active_interface_variables()) {
    if (glsl_compiler.has_decoration(var, spv::DecorationLocation)) {
      auto location =
          glsl_compiler.get_decoration(var, spv::DecorationLocation);
      if (glsl_compiler.get_storage_class(var) == spv::StorageClassInput) {
        if (input_names.count(location) != 0) {
          glsl_compiler.set_name(var, input_names[location]);
        }
      }
      if (glsl_compiler.get_storage_class(var) ==
          spv::StorageClassUniformConstant) {
        if (uniform_names.count(location) != 0) {
          glsl_compiler.set_name(var, uniform_names[location]);
        }
      }
    }
  }

  auto options = glsl_compiler.get_common_options();
  options.version = version;
  glsl_compiler.set_common_options(options);
  return glsl_compiler.compile();
}

class ViewerApp final : public Application {
public:
  ViewerApp(fs::path frag_shader_path)
      : Application("Viewer", 800, 600),
        _frag_shader_path(std::move(frag_shader_path)) {}

private:
  void init_program(const char *frag_src) {
    auto vert_shader =
        std::make_unique<Shader>(vertex_shader_text, GL_VERTEX_SHADER, "vert");
    auto frag_shader =
        std::make_unique<Shader>(frag_src, GL_FRAGMENT_SHADER, "frag");
    GLuint shaders[] = {vert_shader->get(), frag_shader->get()};

    _program = std::make_unique<Program>(shaders, 2);

    last_time = glfwGetTime();
    // now shaders can be released automatically
  }

  void init() override {
    std::vector<Mesh::Vertex> vertices = {
        {{-1.0f, -1.0f, 0.0f}, {}, {}, {0.0f, 0.0f}}, // left-down
        {{1.0f, -1.0f, 0.0f}, {}, {}, {1.0f, 0.0f}},  // right-down
        {{-1.0f, 1.0f, 0.0f}, {}, {}, {0.0f, 1.0f}},  // left-up
        {{1.0f, 1.0f, 0.0f}, {}, {}, {1.0f, 1.0f}}    // right-up
    };

    std::vector<uint32_t> indices = {0, 1, 2, 1, 3, 2};
    _mesh = std::make_unique<Mesh>(vertices.data(),
                                   uint32_t(vertices.size()),
                                   indices.data(),
                                   uint32_t(indices.size()));
    reload_shader();
  }

  void reload_shader() {
    _compile_error = std::nullopt;
    try {
      _frag_shader_source = load_frag_shader(_frag_shader_path);
      init_program(_frag_shader_source.c_str());
    } catch (std::exception &e) {
      _compile_error = e.what();
      init_program(fragment_shader_text);
    }
  }

  void draw_ui() {
    if (ImGui::Button("Screen Shot")) {
      request_screen_shot();
    }
    auto time_per_frame = average_frame_time();
    ImGui::Text(
        "FPS %0.2f(%0.2fms)", 1.0f / time_per_frame, time_per_frame * 1000.0f);
    ImGui::Checkbox("Pause", &pause);
    ImGui::Text("Path: %s", _frag_shader_path.string().c_str());
    if (ImGui::Button("Reload Shader")) {
      reload_shader();
    }
    if (_compile_error.has_value()) {
      ImGui::Text("Shader Compile Error");
      ImGui::TextUnformatted(_compile_error->c_str());
    } else {
      ImGui::Text("Compiled Shader Source");
      ImGui::TextUnformatted(_frag_shader_source.c_str());
    }
  }

  void update() override {
    glClearColor(0.0, 0.0, 0.0, 1.0);

    double cur_time = glfwGetTime();
    if (!pause) {
      acc_time += float(cur_time - last_time);
    }
    last_time = cur_time;

    int width, height;
    glfwGetFramebufferSize(_window, &width, &height);

    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);

    glUseProgram(_program->get());
    glUniform1f(glGetUniformLocation(_program->get(), "iTime"), acc_time);
    glUniform2f(glGetUniformLocation(_program->get(), "iResolution"),
                float(width),
                float(height));
    _mesh->draw();
    draw_ui();
  }

  double last_time = 0.0f;
  bool pause = false;
  float acc_time = 0.0f;
  fs::path _frag_shader_path;
  std::string _frag_shader_source;
  std::optional<std::string> _compile_error;
  std::unique_ptr<Program> _program;
  std::unique_ptr<Mesh> _mesh;
};

int main(int argc, const char **argv) {
  if (argc == 1) {
    std::cout << "please specify input shader file.frag" << std::endl;
    return 0;
  }
  try {
    ViewerApp app(argv[1]);
    app.run();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
}