cmake_minimum_required(VERSION 3.5)

project (cloc)

file(GLOB_RECURSE sources src/*.c)

include_directories(include)

add_executable(cloc ${sources} main.c)

target_compile_options(cloc PUBLIC -g -Wall -Wextra -pedantic)
