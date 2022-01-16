include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src)

set(MAIN_SOURCES main.c)

include_directories(../include)

set(HEADERS ../include/cloc.h)

add_executable(cloc-bin ${HEADERS} ${MAIN_SOURCES})
target_link_libraries(cloc-bin cloc)

target_compile_options(cloc-bin PUBLIC -g -Wall -Wextra -pedantic -O3 -flto)

set_target_properties(cloc-bin PROPERTIES OUTPUT_NAME cloc)
