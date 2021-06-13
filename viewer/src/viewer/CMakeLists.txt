add_executable(viewer main.cpp)

target_link_libraries(viewer PRIVATE common shaderc spirv-cross-glsl)