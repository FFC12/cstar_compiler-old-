cmake_minimum_required(VERSION 3.20)
project(cstar)
set(CMAKE_CXX_STANDARD 17)
#set(CMAKE_BUILD_TYPE Debug)
#set(CMAKE_BUILD_TYPE Release)
file(GLOB SOURCES "src/**/*.cpp")

include_directories(include extern/Catch2/src)

#add_subdirectory(extern/Catch2)

add_executable(${PROJECT_NAME} main.cpp ${SOURCES})
#target_link_libraries(${PROJECT_NAME} Catch2)

