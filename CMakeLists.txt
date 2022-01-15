cmake_minimum_required(VERSION 3.20)
project(CSTAR)
set(CMAKE_CXX_STANDARD 17)
file(GLOB SOURCES "src/**/*.cpp")

include_directories(include)

add_executable(${PROJECT_NAME} main.cpp ${SOURCES})
