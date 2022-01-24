cmake_minimum_required(VERSION 3.20)
project(CSTAR)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Debug)
file(GLOB SOURCES "src/**/*.cpp")

include_directories(include extern/fmt/include extern/Catch2/src)

add_subdirectory(extern/fmt)
#add_subdirectory(extern/Catch2)

add_executable(${PROJECT_NAME} main.cpp ${SOURCES})
target_link_libraries(${PROJECT_NAME} fmt) #Catch2)

